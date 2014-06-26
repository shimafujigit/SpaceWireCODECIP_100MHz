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
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity SpaceWireCODECIPTransmitter is
    generic (
        gInitializeTransmitClockDivideValue : std_logic_vector (5 downto 0) := "001001"
        );

    port (
        transmitClock            : in  std_logic;
        clock                    : in  std_logic;
        receiveClock             : in  std_logic;
        reset                    : in  std_logic;
        spaceWireDataOut         : out std_logic;
        spaceWireStrobeOut       : out std_logic;
        tickIn                   : in  std_logic;
        timeIn                   : in  std_logic_vector (5 downto 0);
        controlFlagsIn           : in  std_logic_vector (1 downto 0);
        transmitDataEnable       : in  std_logic;
        transmitData             : in  std_logic_vector (7 downto 0);
        transmitDataControlFlag  : in  std_logic;
        transmitReady            : out std_logic;
        enableTransmit           : in  std_logic;
        sendNulls                : in  std_logic;
        sendFCTs                 : in  std_logic;
        sendNCharacters          : in  std_logic;
        sendTimeCodes            : in  std_logic;
        gotFCT                   : in  std_logic;
        gotNCharacter            : in  std_logic;
        receiveFIFOCount         : in  std_logic_vector(5 downto 0);
        creditError              : out std_logic;
        transmitClockDivide      : in  std_logic_vector(5 downto 0);
        creditCountOut           : out std_logic_vector (5 downto 0);
        outstandingCountOut      : out std_logic_vector (5 downto 0);
        spaceWireResetOut        : in  std_logic;
        transmitEEPAsynchronous  : out std_logic;
        TransmitEOPAsynchronous  : out std_logic;
        TransmitByteAsynchronous : out std_logic
        );
end SpaceWireCODECIPTransmitter;

architecture Behavioral of SpaceWireCODECIPTransmitter is


    component SpaceWireCODECIPSynchronizeOnePulse is
        port (
            clock             : in  std_logic;
            asynchronousClock : in  std_logic;
            reset             : in  std_logic;
            asynchronousIn    : in  std_logic;
            synchronizedOut   : out std_logic
            );
    end component;

    type transmitStateMachine is (
        transmitStateStop,
        transmitStateParity,
        transmitStateControl,
        transmitStateData
        );

    signal transmitState : transmitStateMachine;

    signal iDivideCount                    : std_logic_vector(5 downto 0);
    signal iDivideState                    : std_logic;
    signal iTransmitParity                 : std_logic;
    signal iNullSend                       : std_logic;
    signal iTimeCodeSend                   : std_logic;
    signal iDataOutRegister                : std_logic;
    signal iStrobeOutRegister              : std_logic;
    signal iSendStart                      : std_logic;
    signal iSendDone                       : std_logic;
    signal iSendData                       : std_logic_vector(8 downto 0);
    signal iSendCount                      : std_logic_vector(3 downto 0);
    signal transmitDataEnableSynchronized  : std_logic;
    signal iDecrementCredit                : std_logic;
    signal iTransmitFCTStart               : std_logic;
    signal iTransmitFCTDone                : std_logic;
    signal tickInSynchronized              : std_logic;
    signal iTransmitTimeCodeStart          : std_logic;
    signal iTransmitTimeCodeDone           : std_logic;
    signal iTransmitTimeCodeState          : std_logic;
    signal gotNCharacterSynchronized       : std_logic;
    signal iGotNCharacterSynchronizedDelay : std_logic_vector (9 downto 0);
    signal iOutstandingCount               : std_logic_vector (5 downto 0);
    signal iReceiveFIFOCountBuffer0        : std_logic_vector (5 downto 0);
    signal iReceiveFIFOCountBuffer1        : std_logic_vector (5 downto 0);
    signal iReceiveFIFOCountBuffer         : std_logic_vector (5 downto 0);
    signal iTransmitFCTState               : std_logic;
    signal iTransmitDataBuffer             : std_logic_vector (7 downto 0);
    signal iTransmitDataControlFlagBuffer  : std_logic;
    signal gotFCTSynchronized              : std_logic;
    signal iTransmitCreditCount            : std_logic_vector (6 downto 0);
    signal iCreditErrorNCharactorOverFlow  : std_logic;
    signal iCreditErrorFCTOverFlow         : std_logic;
    signal iTransmitReady                  : std_logic;
    signal iCreditError                    : std_logic;
    signal iTimeInBuffer                   : std_logic_vector (5 downto 0) := "000000";
    signal iFirstNullSend                  : std_logic;
    signal iResetIn                        : std_logic;
    signal iClockDivideRegister            : std_logic_vector (5 downto 0);
    signal iTransmitEEPAsynchronous        : std_logic;
    signal iTransmitEOPAsynchronous        : std_logic;
    signal iTransmitByteAsynchronous       : std_logic;
    signal iCreditOverFlow                 : std_logic;
    
    
begin

    iResetIn                 <= reset or spaceWireResetOut;
    transmitEEPAsynchronous  <= iTransmitEEPAsynchronous;
    transmitEOPAsynchronous  <= iTransmitEOPAsynchronous;
    transmitByteAsynchronous <= iTransmitByteAsynchronous;


    transmitDataEnablePulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => transmitClock,
            asynchronousClock => Clock,
            reset             => iResetIn,
            asynchronousIn    => transmitDataEnable,
            synchronizedOut   => transmitDataEnableSynchronized
            );

    tickInPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => transmitClock,
            asynchronousClock => Clock,
            reset             => iResetIn,
            asynchronousIn    => tickIn,
            synchronizedOut   => tickInSynchronized
            );

    gotFCTPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => transmitClock,
            asynchronousClock => receiveClock,
            reset             => iResetIn,
            asynchronousIn    => gotFCT,
            synchronizedOut   => gotFCTSynchronized
            );

    gotNCharacterPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => transmitClock,
            asynchronousClock => receiveClock,
            reset             => iResetIn,
            asynchronousIn    => gotNCharacter,
            synchronizedOut   => gotNCharacterSynchronized
            );


    creditError         <= iCreditError;
    iCreditError        <= iCreditErrorNCharactorOverFlow or iCreditErrorFCTOverFlow;
    creditCountOut      <= iTransmitCreditCount (5 downto 0);
    outstandingCountOut <= iOutstandingCount;
    transmitReady       <= iTransmitReady;
    iTransmitReady      <= '0' when (iSendStart = '1' or iTransmitFCTStart = '1' or iTransmitCreditCount = "0000000") else '1';
    spaceWireDataOut    <= iDataOutRegister;
    spaceWireStrobeOut  <= iStrobeOutRegister;



----------------------------------------------------------------------
-- Statistical information, Transmit One Shot Pulse(EOP,EEP,1Byte).
----------------------------------------------------------------------
    process(transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iTransmitEEPAsynchronous  <= '0';
            iTransmitEOPAsynchronous  <= '0';
            iTransmitByteAsynchronous <= '0';
        elsif (transmitClock'event and transmitClock = '1') then
            if (transmitDataEnable = '1') then
                if (transmitDataControlFlag = '1') then
                    
                    if (transmitData(0) = '0') then
                        --EOP Transmit.
                        iTransmitEOPAsynchronous <= '1';
                    else
                        --EEP Transmit.
                        iTransmitEEPAsynchronous <= '1';
                    end if;
                elsif (transmitDataControlFlag = '0') then
                    --1Byte Transmit.
                    iTransmitByteAsynchronous <= '1';
                end if;
            else
                iTransmitEEPAsynchronous  <= '0';
                iTransmitEOPAsynchronous  <= '0';
                iTransmitByteAsynchronous <= '0';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.4.2 Transmitter
-- When the TICK_IN signal is asserted the transmitter sends out a Time-Code
-- as soon as the transmitter has finished sending the current character or 
-- control code. The value of the Time-Code is the value of the TIME_IN and 
-- CONTROL-FLAGS_IN signals at the point in time when TICK_IN is asserted.
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iTransmitTimeCodeState <= '0';
            iTransmitTimeCodeStart <= '0';
            iTimeInBuffer          <= (others => '0');

        elsif (transmitClock'event and transmitClock = '1') then
            if(sendTimeCodes = '1')then
                if (iTransmitTimeCodeState = '0') then
                    if (tickInSynchronized = '1') then
                        iTransmitTimeCodeStart <= '1';
                        iTransmitTimeCodeState <= '1';
                        iTimeInBuffer          <= timeIn;
                    end if;
                else
                    if (iTransmitTimeCodeDone = '1') then
                        iTransmitTimeCodeStart <= '0';
                        iTransmitTimeCodeState <= '0';
                    end if;
                end if;
            end if;
        end if;

    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.3 Flow control (normative)
-- Receives an FCT its transmitter increments the credit count by eight.
-- Whenever the transmitter sends an N-Char it decrements the credit count 
-- by one.
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iTransmitCreditCount <= (others => '0');
        elsif (transmitClock'event and transmitClock = '1') then
            if (gotFCTSynchronized = '1') then
                if (iDecrementCredit = '1') then
                    iTransmitCreditCount <= iTransmitCreditCount + 7;
                else
                    iTransmitCreditCount <= iTransmitCreditCount + 8;
                end if;
            elsif (iDecrementCredit = '1') then
                if (iTransmitCreditCount /= "0000000") then
                    iTransmitCreditCount <= iTransmitCreditCount - 1;
                end if;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.5.3.8 CreditError
-- If an FCT is received when the credit count is at or close to its maximum
-- value (i.e. within eight of the maximum value), the credit count is
-- not incremented and a credit error occurs.
----------------------------------------------------------------------
    process(transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iCreditOverFlow <= '0';
        elsif (transmitClock'event and transmitClock = '1') then
            if (iTransmitCreditCount > "0111000") then
                iCreditOverFlow <= '1';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.5.3.8 CreditError
-- Synchronized Reset the CreditErrorFCTOverFlow.
----------------------------------------------------------------------
    process(transmitClock, iResetIn)
    begin
        if (transmitClock'event and transmitClock = '1') then
            if (iResetIn = '1') then
                iCreditErrorFCTOverFlow <= '0';
            else
                iCreditErrorFCTOverFlow <= iCreditOverFlow and not iCreditErrorFCTOverFlow;
            end if;
        end if;
    end process;


----------------------------------------------------------------------
-- Receive Wait time for subtraction OutstandingCount
-- after adding receiveFIFOCount.
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iGotNCharacterSynchronizedDelay <= (others => '0');
        elsif (transmitClock'event and transmitClock = '1') then
            iGotNCharacterSynchronizedDelay (0)          <= gotNCharacterSynchronized;
            iGotNCharacterSynchronizedDelay (9 downto 1) <= iGotNCharacterSynchronizedDelay (8 downto 0);
        end if;
    end process;

----------------------------------------------------------------------
-- Synchronized input signal to transmitClock. 
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iReceiveFIFOCountBuffer0 <= (others => '0');
            iReceiveFIFOCountBuffer1 <= (others => '0');
            iReceiveFIFOCountBuffer  <= (others => '0');

        elsif (transmitClock'event and transmitClock = '1') then
            iReceiveFIFOCountBuffer0 <= receiveFIFOCount;
            iReceiveFIFOCountBuffer1 <= iReceiveFIFOCountBuffer0;

            if (iReceiveFIFOCountBuffer1 = iReceiveFIFOCountBuffer0) then
                iReceiveFIFOCountBuffer <= iReceiveFIFOCountBuffer1;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.3 Flow control (normative)
-- Each time a link interface receives an FCT its transmitter
-- increments the credit count by eight.
-- Whenever the transmitter sends an N-Char it decrements the credit
-- count by one.
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iOutstandingCount              <= "000000";
            iTransmitFCTState              <= '0';
            iTransmitFCTStart              <= '0';
            iCreditErrorNCharactorOverFlow <= '0';

        elsif (transmitClock'event and transmitClock = '1') then
            if (iTransmitFCTState = '0') then
                if (iOutstandingCount + iReceiveFIFOCountBuffer <= "110000" and sendFCTs = '1') then
                    iTransmitFCTStart <= '1';
                    iTransmitFCTState <= '1';
                end if;
                if (iGotNCharacterSynchronizedDelay (9) = '1') then
                    iOutstandingCount <= iOutstandingCount - 1;
                end if;
            else
                if (iTransmitFCTDone = '1') then
                    if (iGotNCharacterSynchronizedDelay (9) = '1') then
                        iOutstandingCount <= iOutstandingCount + 7;
                    else
                        iOutstandingCount <= iOutstandingCount + 8;
                    end if;
                    iTransmitFCTStart <= '0';
                    iTransmitFCTState <= '0';
                else
                    if (iGotNCharacterSynchronizedDelay (9) = '1') then
                        iOutstandingCount <= iOutstandingCount - 1;
                    end if;
                end if;
            end if;

            ----------------------------------------------------------------------
            -- ECSS-E-ST-50-12C 8.5.3.8 CreditError
            -- Credit error occurs if data is received when the
            -- host system is not expecting any more data.                   
            ----------------------------------------------------------------------
            if (iGotNCharacterSynchronizedDelay (9) = '1' and iOutstandingCount = "000000") then
                iCreditErrorNCharactorOverFlow <= '1';
            else
                iCreditErrorNCharactorOverFlow <= '0';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Instract to start Transmit and load data to buffer after read the data from 
-- TransmitFIFO.
----------------------------------------------------------------------
    process (transmitClock, iResetIn)
    begin
        if (iResetIn = '1') then
            iSendStart                     <= '0';
            iTransmitDataBuffer            <= (others => '0');
            iTransmitDataControlFlagBuffer <= '0';

        elsif (transmitClock'event and transmitClock = '1') then
            if (transmitDataEnableSynchronized = '1') then
                iTransmitDataBuffer            <= transmitData;
                iTransmitDataControlFlagBuffer <= transmitDataControlFlag;
                iSendStart                     <= '1';
            elsif (iSendDone = '1') then
                iSendStart <= '0';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 6.6.5 Initial operating data signalling rate
-- After a reset the SpaceWire link transmitter shall initially commence 
-- operating at a data signalling rate of (10±1) Mb/s.
----------------------------------------------------------------------
    process (transmitClock, reset)
    begin
        if (reset = '1') then
            iClockDivideRegister <= gInitializeTransmitClockDivideValue;
        elsif (transmitClock'event and transmitClock = '1') then
            if (sendNCharacters = '1') then
                iClockDivideRegister <= transmitClockDivide;
            else
                iClockDivideRegister <= gInitializeTransmitClockDivideValue;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.4.3 Transmit clock
-- Dividing counter to determine the Transmit signalling rate.
----------------------------------------------------------------------
    process (transmitClock, reset)
    begin
        if (reset = '1') then
            iDivideCount <= (others => '0');
            iDivideState <= '0';
        elsif (transmitClock'event and transmitClock = '1') then
            if (iDivideCount >= iClockDivideRegister) then
                iDivideCount <= (others => '0');
                iDivideState <= '1';
            else
                iDivideCount <= iDivideCount + 1;
                iDivideState <= '0';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.4.2 Transmitter
-- The data is convoert to serial after stored in shift register, Transmit Tx as 
-- DS signal. 
-- Generate odd parity and Transmit Null data automatically.
----------------------------------------------------------------------
    process (transmitClock, reset)
    begin
        if (reset = '1') then
            iTransmitParity       <= '0';
            transmitState         <= transmitStateStop;
            iDataOutRegister      <= '0';
            iStrobeOutRegister    <= '0';
            iNullSend             <= '0';
            iTimeCodeSend         <= '0';
            iSendDone             <= '0';
            iTransmitFCTDone      <= '0';
            iTransmitTimeCodeDone <= '0';
            iDecrementCredit      <= '0';
            iFirstNullSend        <= '0';
            iSendCount            <= (others => '0');
            iSendData             <= (others => '0');
            

        elsif (transmitClock'event and transmitClock = '1') then
            if (iDivideState = '1') then
                case transmitState is
                    
                    when transmitStateStop =>
                        if (enableTransmit = '1' and sendNulls = '1') then
                            transmitState <= transmitStateParity;
                        else
                            iTransmitParity    <= '0';
                            iDataOutRegister   <= '0';
                            iStrobeOutRegister <= '0';
                            iNullSend          <= '0';
                            iFirstNullSend     <= '0';
                        end if;

                    ----------------------------------------------------------------------
                    -- Odd Parity Generate.
                    ----------------------------------------------------------------------
                    when transmitStateParity =>
                        if (enableTransmit = '1') then
                            if (iNullSend = '1') then
                                -- send pending FCT of NULL(ESC+FCT)
                                iSendData  <= "000000" & "001";
                                iSendCount <= x"2";
                                if (iDataOutRegister = iTransmitParity) then
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= not iStrobeOutRegister;
                                else
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= iStrobeOutRegister;
                                end if;
                                iNullSend <= '0';

                            elsif (iTimeCodeSend = '1') then
                                -- send pending TIME of TCODE(ESC+TIME)
                                iSendData  <= controlFlagsIn & iTimeInBuffer & '0';
                                iSendCount <= x"8";
                                if (iDataOutRegister = (iTransmitParity xor '1')) then
                                    iDataOutRegister   <= iTransmitParity xor '1';
                                    iStrobeOutRegister <= not iStrobeOutRegister;
                                else
                                    iDataOutRegister   <= iTransmitParity xor '1';
                                    iStrobeOutRegister <= iStrobeOutRegister;
                                end if;
                                iTimeCodeSend         <= '0';
                                iTransmitTimeCodeDone <= '1';

                            elsif (iTransmitTimeCodeStart = '1') then
                                -- send ESC of TCODE.
                                iSendData  <= "000000" & "111";
                                iSendCount <= x"2";
                                if (iDataOutRegister = iTransmitParity) then
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= not iStrobeOutRegister;
                                else
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= iStrobeOutRegister;
                                end if;
                                iTimeCodeSend <= '1';

                            elsif (sendFCTs = '1' and iTransmitFCTStart = '1' and iFirstNullSend = '1') then
                                -- send FCT.
                                iSendData  <= "000000" & "001";
                                iSendCount <= x"2";
                                if (iDataOutRegister = iTransmitParity) then
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= not iStrobeOutRegister;
                                else
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= iStrobeOutRegister;
                                end if;
                                iTransmitFCTDone <= '1';

                            elsif (sendNCharacters = '1' and iSendStart = '1') then
                                iDecrementCredit <= '1';
                                if (iTransmitDataControlFlagBuffer = '1' and iTransmitDataBuffer(0) = '0') then
                                    -- send EOP.
                                    iSendData  <= "0" & "00000" & "101";
                                    iSendCount <= x"2";
                                    if (iDataOutRegister = iTransmitParity) then
                                        iDataOutRegister   <= iTransmitParity;
                                        iStrobeOutRegister <= not iStrobeOutRegister;
                                    else
                                        iDataOutRegister   <= iTransmitParity;
                                        iStrobeOutRegister <= iStrobeOutRegister;
                                    end if;
                                elsif (iTransmitDataControlFlagBuffer = '1' and iTransmitDataBuffer(0) = '1') then
                                    -- send EEP.
                                    iSendData  <= "0" & "00000" & "011";
                                    iSendCount <= x"2";
                                    if (iDataOutRegister = iTransmitParity) then
                                        iDataOutRegister   <= iTransmitParity;
                                        iStrobeOutRegister <= not iStrobeOutRegister;
                                    else
                                        iDataOutRegister   <= iTransmitParity;
                                        iStrobeOutRegister <= iStrobeOutRegister;
                                    end if;
                                else
                                    -- send 8-bit data.
                                    iSendData  <= iTransmitDataBuffer & '0';
                                    iSendCount <= x"8";
                                    if (iDataOutRegister = (iTransmitParity xor '1')) then
                                        iDataOutRegister   <= iTransmitParity xor '1';
                                        iStrobeOutRegister <= not iStrobeOutRegister;
                                    else
                                        iDataOutRegister   <= iTransmitParity xor '1';
                                        iStrobeOutRegister <= iStrobeOutRegister;
                                    end if;
                                end if;
                                iSendDone <= '1';

                            elsif (sendNulls = '1') then
                                -- send ESC of NULL.
                                iSendData  <= "000000" & "111";
                                iSendCount <= x"2";
                                if (iDataOutRegister = iTransmitParity) then
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= not iStrobeOutRegister;
                                else
                                    iDataOutRegister   <= iTransmitParity;
                                    iStrobeOutRegister <= iStrobeOutRegister;
                                end if;
                                iNullSend      <= '1';
                                iFirstNullSend <= '1';
                            end if;
                            transmitState <= transmitStateControl;
                        else
                            iDataOutRegister <= '0';
                            transmitState    <= transmitStateStop;
                        end if;

                    ----------------------------------------------------------------------
                    -- Transmit Data Control Flag
                    -- Data Character = "0" Control Caracter = "1".
                    ----------------------------------------------------------------------
                    when transmitStateControl =>
                        if (enableTransmit = '1') then
                            iSendDone             <= '0';
                            iDecrementCredit      <= '0';
                            iTransmitFCTDone      <= '0';
                            iTransmitTimeCodeDone <= '0';
                            iSendCount            <= iSendCount - 1;
                            if (iDataOutRegister = iSendData(0)) then
                                iDataOutRegister   <= iSendData(0);
                                iStrobeOutRegister <= not iStrobeOutRegister;
                            else
                                iDataOutRegister   <= iSendData(0);
                                iStrobeOutRegister <= iStrobeOutRegister;
                            end if;
                            iSendData       <= '0' & iSendData(8 downto 1);
                            iTransmitParity <= '0';
                            transmitState   <= transmitStateData;
                        else
                            iDataOutRegister <= '0';
                            transmitState    <= transmitStateStop;
                        end if;

                    ----------------------------------------------------------------------
                    -- Transmit Data Character or Control Caracter.
                    ----------------------------------------------------------------------
                    when transmitStateData =>
                        if (enableTransmit = '1') then
                            if (iDataOutRegister = iSendData(0)) then
                                iDataOutRegister   <= iSendData(0);
                                iStrobeOutRegister <= not iStrobeOutRegister;
                            else
                                iDataOutRegister   <= iSendData(0);
                                iStrobeOutRegister <= iStrobeOutRegister;
                            end if;
                            iTransmitParity <= iTransmitParity xor iSendData(0);
                            iSendData       <= '0' & iSendData(8 downto 1);
                            if (iSendCount = "0000") then
                                transmitState <= transmitStateParity;
                            else
                                iSendCount <= iSendCount - 1;
                            end if;
                        else
                            iDataOutRegister <= '0';
                            transmitState    <= transmitStateStop;
                        end if;
                    when others => null;

                end case;
            else
                iTransmitFCTDone <= '0';
                iSendDone        <= '0';
                iDecrementCredit <= '0';
            end if;
        end if;
    end process;
end Behavioral;

