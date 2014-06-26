------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) <2013> <Shimafuji Electric Inc., Osaka University, JAXA>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity SpaceWireCODECIPReceiverSynchronize is
    generic (
        gDisconnectCountValue : integer := 141
        );
    port (
        spaceWireStrobeIn       : in  std_logic;
        spaceWireDataIn         : in  std_logic;
        receiveDataOut          : out std_logic_vector (8 downto 0);
        receiveDataValidOut     : out std_logic;
        receiveTimeCodeOut      : out std_logic_vector (7 downto 0);
        receiveTimeCodeValidOut : out std_logic;
        receiveNCharacterOut    : out std_logic;
        receiveFCTOut           : out std_logic;
        receiveNullOut          : out std_logic;
        receiveEEPOut           : out std_logic;
        receiveEOPOut           : out std_logic;
        receiveOffOut           : out std_logic;
        receiverErrorOut        : out std_logic;
        parityErrorOut          : out std_logic;
        escapeErrorOut          : out std_logic;
        disconnectErrorOut      : out std_logic;
        spaceWireReset          : in  std_logic;
        receiveFIFOWriteEnable  : out std_logic;
        enableReceive           : in  std_logic;
        receiveClock            : in  std_logic
        );
end SpaceWireCODECIPReceiverSynchronize;

architecture RTL of SpaceWireCODECIPReceiverSynchronize is

    signal iDataRegister           : std_logic_vector(7 downto 0);
    signal iParity                 : std_logic;
    signal iESCFlag                : std_logic;
    signal iSpaceWireSynchronize   : std_logic_vector(1 downto 0);
    signal iBitCount               : std_logic_vector(3 downto 0);
    signal iLinkTimeOutCounter     : std_logic_vector (7 downto 0);
    signal iDisconnectErrorOut     : std_logic;
    signal iParityErrorOut         : std_logic;
    signal iEscapeErrorOut         : std_logic;
    signal iCommandFlag, iDataFlag : std_logic;
    
    type spaceWireStateMachine is (
        spaceWireIdel,
        spaceWireOff,
        spaceWireEven0,
        spaceWireEven1,
        spaceWireWaitEven,
        spaceWireOdd0,
        spaceWireOdd1,
        spaceWireWaitOdd
        );

    signal spaceWireState : spaceWireStateMachine;

    signal iReceiverEOPOut       : std_logic;
    signal iReceiverEEPOut       : std_logic;
    signal iReceiverDataValidOut : std_logic;

    signal iReceiveDataOut          : std_logic_vector (8 downto 0) := (others => '0');
    signal iReceiveTimeCodeOut      : std_logic_vector (7 downto 0);
    signal iReceiveTimeCodeValidOut : std_logic;
    signal iReceiveNCharacterOut    : std_logic;
    signal iReceiveFCTOut           : std_logic;
    signal iReceiveNullOut          : std_logic;
    signal iReceiveOffOut           : std_logic;
    signal iReceiverErrorOut        : std_logic;
    signal iReceiveFIFOWriteEnable  : std_logic;

begin

    receiveDataOut          <= iReceiveDataOut;
    receiveTimeCodeOut      <= iReceiveTimeCodeOut;
    receiveTimeCodeValidOut <= iReceiveTimeCodeValidOut;
    receiveNCharacterOut    <= iReceiveNCharacterOut;
    receiveFCTOut           <= iReceiveFCTOut;
    receiveNullOut          <= iReceiveNullOut;
    receiveOffOut           <= iReceiveOffOut;
    receiverErrorOut        <= iReceiverErrorOut;
    receiveFIFOWriteEnable  <= iReceiveFIFOWriteEnable;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.4.4 Receiver.
----------------------------------------------------------------------

----------------------------------------------------------------------
-- synchronize DS signal to the receiveClock.
----------------------------------------------------------------------
    process (receiveClock)
    begin
        if (receiveClock'event and receiveClock = '1') then
            iSpaceWireSynchronize <= spaceWireStrobeIn & spaceWireDataIn;
        end if;
    end process;

----------------------------------------------------------------------
-- Detect a change of the DS signal.
----------------------------------------------------------------------
    process (receiveClock, spaceWireReset, iDisconnectErrorOut)
    begin
        if (spaceWireReset = '1' or iDisconnectErrorOut = '1') then
            spaceWireState <= spaceWireIdel;
        elsif (receiveClock'event and receiveClock = '1') then
            if(enableReceive = '1')then
                if (spaceWireState = spaceWireIdel) then
                    if (iSpaceWireSynchronize = "00") then
                        spaceWireState <= spaceWireOff;
                    end if;
                elsif (spaceWireState = spaceWireOff) then
                    if (iSpaceWireSynchronize = "10") then
                        spaceWireState <= spaceWireOdd0;
                    end if;
                elsif (spaceWireState = spaceWireEven1 or spaceWireState = spaceWireEven0 or spaceWireState = spaceWireWaitOdd) then
                    if (iSpaceWireSynchronize = "10") then
                        spaceWireState <= spaceWireOdd0;
                    elsif (iSpaceWireSynchronize = "01") then
                        spaceWireState <= spaceWireOdd1;
                    else
                        spaceWireState <= spaceWireWaitOdd;
                    end if;
                elsif (spaceWireState = spaceWireOdd1 or spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireWaitEven) then
                    if (iSpaceWireSynchronize = "00") then
                        spaceWireState <= spaceWireEven0;
                    elsif (iSpaceWireSynchronize = "11") then
                        spaceWireState <= spaceWireEven1;
                    else
                        spaceWireState <= spaceWireWaitEven;
                    end if;
                else
                    spaceWireState <= spaceWireIdel;
                end if;
            end if;
        end if;
    end process;


    process (receiveClock)
    begin
        if (receiveClock'event and receiveClock = '1') then
            ----------------------------------------------------------------------
            -- Take the data into the shift register on the State transition of spaceWireState.
            ----------------------------------------------------------------------
            if(enableReceive = '1')then
                if (spaceWireState = spaceWireOff) then
                    iDataRegister <= (others => '0');
                elsif (spaceWireState = spaceWireOdd1 or spaceWireState = spaceWireEven1) then
                    iDataRegister <= '1' & iDataRegister(7 downto 1);
                elsif (spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireEven0) then
                    iDataRegister <= '0' & iDataRegister(7 downto 1);
                end if;
            else
                iDataRegister <= (others => '0');
            end if;

            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 7.4 Parity for error detection.
            -- Odd Parity.
            ----------------------------------------------------------------------
            if(enableReceive = '1' and iEscapeErrorOut = '0' and iDisconnectErrorOut = '0')then
                if (spaceWireState = spaceWireOff) then
                    iParity <= '0';
                elsif (iBitCount = 0 and spaceWireState = spaceWireEven1) then
                    if (iParity = '1') then
                        iParityErrorOut <= '1';
                        iParity         <= '0';
                    end if;
                elsif (iBitCount = 0 and spaceWireState = spaceWireEven0) then
                    if iParity = '0' then
                        iParityErrorOut <= '1';
                    else
                        iParity <= '0';
                    end if;
                elsif (spaceWireState = spaceWireOdd1 or spaceWireState = spaceWireEven1) then
                    iParity <= not iParity;
                end if;
            else
                iParityErrorOut <= '0';
            end if;

            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 8.5.3.7.2 Disconnect error.
            -- Disconnect error is an error condition asserted
            -- when the length of time since the last transition on
            -- the D or S lines was longer than 850 ns nominal.
            ----------------------------------------------------------------------
            if(enableReceive = '1' and iEscapeErrorOut = '0' and iParityErrorOut = '0')then
                if (spaceWireState = spaceWireWaitOdd or spaceWireState = spaceWireWaitEven) then
                    if (iLinkTimeOutCounter < gDisconnectCountValue) then
                        iLinkTimeOutCounter <= iLinkTimeOutCounter + 1;
                    else
                        iDisconnectErrorOut <= '1';
                    end if;
                elsif (spaceWireState = spaceWireIdel) then
                    iLinkTimeOutCounter <= X"00";
                    
                elsif (spaceWireState = spaceWireOdd1 or spaceWireState = spaceWireEven1 or spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireEven0) then
                    iLinkTimeOutCounter <= X"00";
                end if;
            else
                iDisconnectErrorOut <= '0';
                iLinkTimeOutCounter <= X"00";
            end if;
            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 4.4 Character level
            -- ECSS-E-ST-50-12C 7.2 Data characters
            -- Discriminate the data character or the  the control character by the Data 
            -- Control Flag.
            ----------------------------------------------------------------------
            if(enableReceive = '1')then
                if (spaceWireState = spaceWireIdel) then
                    iCommandFlag <= '0'; iDataFlag <= '0';
                elsif (iBitCount = 0 and spaceWireState = spaceWireEven0) then
                    iCommandFlag <= '0'; iDataFlag <= '1';
                elsif (iBitCount = 0 and spaceWireState = spaceWireEven1) then
                    iCommandFlag <= '1'; iDataFlag <= '0';
                end if;
            else
                iCommandFlag <= '0'; iDataFlag <= '0';
            end if;
            ----------------------------------------------------------------------
            -- Increment bit of character corresponding by state transition of 
            -- spaceWireState.
            ----------------------------------------------------------------------
            if(enableReceive = '1' and iEscapeErrorOut = '0' and iDisconnectErrorOut = '0')then
                if (spaceWireState = spaceWireIdel or spaceWireState = spaceWireOff) then
                    iBitCount <= X"0";
                elsif (spaceWireState = spaceWireEven1 or spaceWireState = spaceWireEven0) then
                    if (iBitCount = 1 and iCommandFlag = '1') then
                        iBitCount <= X"0";
                    elsif (iBitCount = 4 and iCommandFlag = '0') then
                        iBitCount <= X"0";
                    else
                        iBitCount <= iBitCount + 1;
                    end if;
                end if;
            else
                iBitCount <= X"0";
            end if;

            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 7.3 Control characters and control codes.
            -- Discriminate  Data character, Control code and Time corde, and write to 
            -- Receive buffer
            ----------------------------------------------------------------------
            if(enableReceive = '1')then
                if (iBitCount = 0 and (spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireOdd1)) then
                    if (iDataFlag = '1') then
                        if (iESCFlag = '1') then
                            --Time Code Receive.
                            iReceiveTimeCodeOut <= iDataRegister;
                        else
                            --Data Receive.
                            iReceiveDataOut         <= '0' & iDataRegister;
                            iReceiveFIFOWriteEnable <= '1';
                        end if;
                    elsif (iCommandFlag = '1') then
						if (iDataRegister (7 downto 6) = "10") then     --EOP
							iReceiveDataOut <= '1' & "00000000";
						elsif (iDataRegister (7 downto 6) = "01") then  --EEP
							iReceiveDataOut <= '1' & "00000001";
						end if;

                        if ((iESCFlag /= '1') and (iDataRegister (7 downto 6) = "10" or iDataRegister (7 downto 6) = "01")) then
                            --EOP EEP Receive.
                            iReceiveFIFOWriteEnable <= '1';
                        end if;
                    end if;
                else
                    iReceiveFIFOWriteEnable <= '0';
                end if;
            end if;

            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 7.3 Control characters and control codes.
            -- ECSS-E-ST-50-12C 8.5.3.7.4 Escape error.
            -- Receive DataCharacter, ControlCode and TimeCode.
            ----------------------------------------------------------------------
            if(enableReceive = '1' and iDisconnectErrorOut = '0' and iParityErrorOut = '0')then
                if (iBitCount = 0 and (spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireOdd1)) then
                    if (iCommandFlag = '1') then
                        case iDataRegister(7 downto 6) is

                            ----------------------------------------------------------------------
                            -- ECSS-E-ST-50-12C 8.5.3.2 gotNULL.
                            -- ECSS-E-ST-50-12C 8.5.3.3 gotFCT.
                            ----------------------------------------------------------------------
                            when "00" =>  -- FCT Receive or Null Receive.
                                if (iESCFlag = '1') then
                                    iReceiveNullOut <= '1';
                                    iESCFlag        <= '0';
                                else
                                    iReceiveFCTOut <= '1';
                                end if;
                                
                            when "11" =>  -- ESC Receive.
                                if (iESCFlag = '1') then
                                    iEscapeErrorOut <= '1';
                                else
                                    iESCFlag <= '1';
                                end if;

                            when "10" =>  -- EOP Receive.
                                if (iESCFlag = '1') then
                                    iEscapeErrorOut <= '1';
                                else
                                    iReceiverEOPOut <= '1';
                                end if;

                            when "01" =>  -- EEP Receive.
                                if (iESCFlag = '1') then
                                    iEscapeErrorOut <= '1';
                                else
                                    iReceiverEEPOut <= '1';
                                end if;
                            when others => null;
                        end case;

                    ----------------------------------------------------------------------
                    -- ECSS-E-ST-50-12C 8.5.3.5 gotTime-Code.
                    -- ECSS-E-ST-50-12C 8.5.3.4 gotN-Char.
                    ----------------------------------------------------------------------
                    elsif (iDataFlag = '1') then
                        if (iESCFlag = '1') then  --TimeCode_Receive.
                            iReceiveTimeCodeValidOut <= '1';
                            iESCFlag                 <= '0';
                        else                      --N-Char_Receive.
                            iReceiverDataValidOut <= '1';
                        end if;
                    end if;

                ----------------------------------------------------------------------
                -- Clear the previous Receive flag before receiving data.
                ----------------------------------------------------------------------
                elsif (iBitCount = 1 and (spaceWireState = spaceWireOdd0 or spaceWireState = spaceWireOdd1)) then
                    iReceiverDataValidOut    <= '0';
                    iReceiveTimeCodeValidOut <= '0';
                    iReceiveNullOut          <= '0';
                    iReceiveFCTOut           <= '0';
                    iReceiverEOPOut          <= '0';
                    iReceiverEEPOut          <= '0';
                elsif spaceWireState = spaceWireIdel then
                    iReceiverDataValidOut    <= '0';
                    iReceiveTimeCodeValidOut <= '0';
                    iReceiveNullOut          <= '0';
                    iReceiveFCTOut           <= '0';
                    iReceiverEOPOut          <= '0';
                    iReceiverEEPOut          <= '0';
                    iEscapeErrorOut          <= '0';
                    iESCFlag                 <= '0';
                end if;
                
            else
                iReceiverDataValidOut    <= '0';
                iReceiveTimeCodeValidOut <= '0';
                iReceiveNullOut          <= '0';
                iReceiveFCTOut           <= '0';
                iReceiverEOPOut          <= '0';
                iReceiverEEPOut          <= '0';
                iEscapeErrorOut          <= '0';
                iESCFlag                 <= '0';
            end if;
        end if;
    end process;


    iReceiveOffOut        <= '1' when spaceWireState = spaceWireOff                                                 else '0';
    iReceiverErrorOut     <= '1' when iDisconnectErrorOut = '1' or iParityErrorOut = '1' or iEscapeErrorOut = '1'   else '0';
    iReceiveNCharacterOut <= '1' when iReceiverEOPOut = '1' or iReceiverEEPOut = '1' or iReceiverDataValidOut = '1' else '0';
    receiveDataValidOut   <= iReceiverDataValidOut;
    receiveEOPOut         <= iReceiverEOPOut;
    receiveEEPOut         <= iReceiverEEPOut;
    parityErrorOut        <= iParityErrorOut;
    escapeErrorOut        <= iEscapeErrorOut;
    disconnectErrorOut    <= iDisconnectErrorOut;


end RTL;
