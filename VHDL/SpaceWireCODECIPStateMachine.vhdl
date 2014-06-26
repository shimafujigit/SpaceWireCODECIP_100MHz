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

entity SpaceWireCODECIPStateMachine is
    port (
        Clock                         : in  std_logic;
        receiveClock                  : in  std_logic;
        reset                         : in  std_logic;
        after12p8us                   : in  std_logic;
        after6p4us                    : in  std_logic;
        linkStart                     : in  std_logic;
        linkDisable                   : in  std_logic;
        autoStart                     : in  std_logic;
        enableTransmit                : out std_logic;
        sendNulls                     : out std_logic;
        sendFCTs                      : out std_logic;
        sendNCharacter                : out std_logic;
        sendTimeCodes                 : out std_logic;
        gotFCT                        : in  std_logic;
        gotTimeCode                   : in  std_logic;
        gotNCharacter                 : in  std_logic;
        gotNull                       : in  std_logic;
        gotBit                        : in  std_logic;
        creditError                   : in  std_logic;
        receiveError                  : in  std_logic;
        enableReceive                 : out std_logic;
        characterSequenceError        : out std_logic;
        spaceWireResetOut             : out std_logic;
        FIFOAvailable                 : in  std_logic;
        timer6p4usReset               : out std_logic;
        timer12p8usStart              : out std_logic;
        linkUpTransitionSynchronize   : out std_logic;
        linkDownTransitionSynchronize : out std_logic;
        linkUpEnable                  : out std_logic;
        nullSynchronize               : out std_logic;
        fctSynchronize                : out std_logic
        );
end SpaceWireCODECIPStateMachine;

architecture Behavioral of SpaceWireCODECIPStateMachine is

    component SpaceWireCODECIPSynchronizeOnePulse is
        port (
            clock             : in  std_logic;
            asynchronousClock : in  std_logic;
            reset             : in  std_logic;
            asynchronousIn    : in  std_logic;
            synchronizedOut   : out std_logic
            );
    end component;

    type linkStateMachine is (
        linkStateErrorReset,
        linkStateErrorWait,
        linkStateReady,
        linkStateStarted,
        linkStateConnecting,
        linkStateRun
        );

    signal linkState                : linkStateMachine;
    signal gotNullSynchronize       : std_logic;
    signal gotFCTSynchronize        : std_logic;
    signal gotTimeCodeSynchronize   : std_logic;
    signal gotNCharacterSynchronize : std_logic;
    signal iAsynchronousError       : std_logic;
    signal receiveErrorsSynchronize : std_logic;
    signal iCharacterSequenceError  : std_logic;
    signal iEnableTransmit          : std_logic;
    signal iSendNulls               : std_logic;
    signal iSendFCTs                : std_logic;
    signal iSendNCharacter          : std_logic;
    signal iSendTimeCodes           : std_logic;
    signal iEnableReceive           : std_logic;
    signal iSpaceWireResetOut       : std_logic;
    signal iTimer6p4usReset         : std_logic;
    signal iTimer12p8usStart        : std_logic;
--
    signal iLinkUpTransition        : std_logic;
    signal iLinkDownTransition      : std_logic;
    signal iLinkUpEnable            : std_logic;
    signal creditSynchronize        : std_logic;
    
begin

    gotNullPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => Clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => gotNull,
            synchronizedOut   => gotNullSynchronize
            );

    gotFCTPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => Clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => gotFCT,
            synchronizedOut   => gotFCTSynchronize
            );

    gotTimeCodePulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => Clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => gotTimeCode,
            synchronizedOut   => gotTimeCodeSynchronize
            );

    gotNCharacterPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => Clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => gotNCharacter,
            synchronizedOut   => gotNCharacterSynchronize
            );

    iAsynchronousError <= receiveErrorsSynchronize;  --

    errorPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => Clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => receiveError,
            synchronizedOut   => receiveErrorsSynchronize
            );

    characterSequenceError        <= iCharacterSequenceError;
    enableTransmit                <= iEnableTransmit;
    sendNulls                     <= iSendNulls;
    sendFCTs                      <= iSendFCTs;
    sendNCharacter                <= iSendNCharacter;
    sendTimeCodes                 <= iSendTimeCodes;
    enableReceive                 <= iEnableReceive;
    spaceWireResetOut             <= iSpaceWireResetOut;
    timer6p4usReset               <= iTimer6p4usReset;
    timer12p8usStart              <= iTimer12p8usStart;
    linkUpTransitionSynchronize   <= iLinkUpTransition;
    linkDownTransitionSynchronize <= iLinkDownTransition;
    linkUpEnable                  <= iLinkUpEnable;
    nullSynchronize               <= gotNullSynchronize;
    fctSynchronize                <= gotFCTSynchronize;


----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.4.6   StateMachine.
-- ECSS-E-ST-50-12C 8.5.3.7 RxErr.
-- ECSS-E-ST-50-12C 8.5.3.8 CreditError.
----------------------------------------------------------------------
    process (Clock, reset, creditError)
    begin
        if (reset = '1' or creditError = '1') then
            linkState               <= linkStateErrorReset;
            iSpaceWireResetOut      <= '1';
            iEnableReceive          <= '0';
            iEnableTransmit         <= '0';
            iSendNulls              <= '0';
            iSendFCTs               <= '0';
            iSendNCharacter         <= '0';
            iSendTimeCodes          <= '0';
            iCharacterSequenceError <= '0';
            iTimer6p4usReset        <= '1';
            iTimer12p8usStart       <= '0';
            iLinkDownTransition     <= '0';
            iLinkUpTransition       <= '0';
            iLinkUpEnable           <= '0';
            
        elsif (Clock'event and Clock = '1') then
            case linkState is

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.2 ErrorReset.
                -- When the reset signal is de-asserted the ErrorReset state shall be left
                -- unconditionally after a delay of 6,4 us (nominal) and the state machine
                -- shall move to the ErrorWait state.
                ----------------------------------------------------------------------
                when linkStateErrorReset =>
                    iLinkUpEnable <= '0';
                    if (iSendTimeCodes = '1') then
                        iLinkDownTransition <= '1';
                    else
                        iLinkDownTransition <= '0';
                    end if;

                    if (FIFOAvailable = '1') then
                        iTimer6p4usReset <= '0';
                    end if;
                    iSpaceWireResetOut      <= '1';
                    iEnableReceive          <= '0';
                    iEnableTransmit         <= '0';
                    iSendNulls              <= '0';
                    iSendFCTs               <= '0';
                    iSendNCharacter         <= '0';
                    iSendTimeCodes          <= '0';
                    iCharacterSequenceError <= '0';

                    if (receiveErrorsSynchronize = '1') then
                        linkState <= linkStateErrorReset;
                    elsif (after6p4us = '1') then
                        iTimer12p8usStart <= '1';
                        linkState         <= linkStateErrorWait;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.3 ErrorWait.
                -- The ErrorWait state shall be left unconditionally after a delay of 12,8 us
                -- (nominal) and the state machine shall move to the Ready state.
                -- If, while in the ErrorWait state, a disconnection error is detected
                -- the state machine shall move back to the ErrorReset state.
                ----------------------------------------------------------------------
                when linkStateErrorWait =>
                    iSpaceWireResetOut <= '0';
                    iTimer12p8usStart  <= '0';
                    iEnableReceive     <= '1';

                    if (receiveErrorsSynchronize = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (gotTimeCodeSynchronize = '1' or gotFCTSynchronize = '1' or gotNCharacterSynchronize = '1') then
                        iCharacterSequenceError <= '1';
                        iTimer6p4usReset        <= '1';
                        linkState               <= linkStateErrorReset;
                    elsif (after12p8us = '1') then
                        linkState <= linkStateReady;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.4 Ready.
                -- The state machine shall wait in the Ready state until the [Link Enabled]
                -- guard becomes true and then it shall move on into the Started state.
                -- If, while in the Ready state, a disconnection error is detected, or if 
                -- after thegotNULL condition is set, a parity error or escape error occurs, 
                -- or any character other than a NULL is received, then the state machine 
                -- shall move to the ErrorReset state.
                ----------------------------------------------------------------------
                when linkStateReady =>
                    iEnableReceive <= '1';
                    if (receiveErrorsSynchronize = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (gotFCTSynchronize = '1' or gotNCharacterSynchronize = '1' or gotTimeCodeSynchronize = '1') then
                        iCharacterSequenceError <= '1';
                        iTimer6p4usReset        <= '1';
                        linkState               <= linkStateErrorReset;
                    elsif (autoStart = '1' and gotNullSynchronize = '1') then
                        iTimer12p8usStart <= '1';
                        linkState         <= linkStateStarted;
                    elsif (linkStart = '1') then
                        iTimer12p8usStart <= '1';
                        linkState         <= linkStateStarted;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.5 Started.
                -- The state machine shall move to the Connecting state if the gotNULL
                -- condition is set.
                -- If, while in the Started state, a disconnection error is detected, or if 
                -- after the gotNULL condition is set, a parity error or escape error occurs, 
                -- or any character other than a NULL is received, then the state machine shall
                -- move to the ErrorReset state.
                ----------------------------------------------------------------------
                when linkStateStarted =>
                    iEnableTransmit   <= '1';
                    iEnableReceive    <= '1';
                    iSendNulls        <= '1';
                    iTimer12p8usStart <= '0';

                    if (receiveErrorsSynchronize = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (linkDisable = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (gotFCTSynchronize = '1' or gotNCharacterSynchronize = '1' or gotTimeCodeSynchronize = '1') then
                        iCharacterSequenceError <= '1';
                        iTimer6p4usReset        <= '1';
                        linkState               <= linkStateErrorReset;
                    elsif (after12p8us = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (gotNullSynchronize = '1') then
                        iTimer12p8usStart <= '1';
                        linkState         <= linkStateConnecting;
                    end if;


                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.6 Connecting
                -- If an FCT is received (gotFCT condition true) the state machine shall
                -- move to the Run state.
                -- If, while in the Connecting state, a disconnect error, parity error or 
                -- escape error is detected, or if any character other than NULL or 
                -- FCT is received, then the state machine shall move to the ErrorReset 
                -- state.
                ----------------------------------------------------------------------
                when linkStateConnecting =>
                    iTimer12p8usStart <= '0';
                    iEnableTransmit   <= '1';
                    iEnableReceive    <= '1';
                    iSendFCTs         <= '1';

                    if (receiveErrorsSynchronize = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (linkDisable = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (after12p8us = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;
                    elsif (gotNCharacterSynchronize = '1') then
                        iCharacterSequenceError <= '1';
                        iTimer6p4usReset        <= '1';
                        linkState               <= linkStateErrorReset;
                    elsif (gotFCTSynchronize = '1') then
                        linkState <= linkStateRun;
                    end if;

                ----------------------------------------------------------------------
                -- ECSS-E-ST-50-12C 8.5.2.7 Run
                -- In the Run state the receiver is enabled and the transmitter is 
                -- enabled to send Time-Codes, FCTs, N-Chars and NULLs.
                -- If  a disconnection error, parity error, ESC error occur, then the state machine
                -- shall move to the ErrorResetState.
                ----------------------------------------------------------------------
                when linkStateRun =>
                    iEnableTransmit <= '1';
                    iEnableReceive  <= '1';
                    iSendNCharacter <= '1';
                    iSendTimeCodes  <= '1';
                    iLinkUpEnable   <= '1';

                    if (iSendTimeCodes = '0') then
                        iLinkUpTransition <= '1';
                    else
                        iLinkUpTransition <= '0';
                    end if;

                    if (linkDisable = '1' or receiveErrorsSynchronize = '1') then
                        iTimer6p4usReset <= '1';
                        linkState        <= linkStateErrorReset;

                    end if;
                when others => null;
            end case;
        end if;
    end process;

end Behavioral;
