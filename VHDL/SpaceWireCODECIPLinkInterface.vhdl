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

library work;
use work.SpaceWireCODECIPPackage.all;


entity SpaceWireCODECIPLinkInterface is
    generic (
        gDisconnectCountValue     : integer                       := 141;
        gTimer6p4usValue          : integer                       := 640;
        gTimer12p8usValue         : integer                       := 1280;
        gTransmitClockDivideValue : std_logic_vector (5 downto 0) := "001001"
        );
    port (
        clock                       : in  std_logic;
        reset                       : in  std_logic;
        -- state machine.
        transmitClock               : in  std_logic;
        linkStart                   : in  std_logic;
        linkDisable                 : in  std_logic;
        autoStart                   : in  std_logic;
        linkStatus                  : out std_logic_vector (15 downto 0);
        errorStatus                 : out std_logic_vector (7 downto 0);
        spaceWireResetOut           : out std_logic;
        FIFOAvailable               : in  std_logic;
        -- transmitter.
        tickIn                      : in  std_logic;
        timeIn                      : in  std_logic_vector (5 downto 0);
        controlFlagsIn              : in  std_logic_vector (1 downto 0);
        transmitDataEnable          : in  std_logic;
        transmitData                : in  std_logic_vector (7 downto 0);
        transmitDataControlFlag     : in  std_logic;
        transmitReady               : out std_logic;
        transmitClockDivideValue    : in  std_logic_vector(5 downto 0);
        creditCount                 : out std_logic_vector (5 downto 0);
        outstndingCount             : out std_logic_vector (5 downto 0);
        -- receiver.
        receiveClock                : in  std_logic;
        tickOut                     : out std_logic;
        timeOut                     : out std_logic_vector (5 downto 0);
        controlFlagsOut             : out std_logic_vector (1 downto 0);
        receiveFIFOWriteEnable1     : out std_logic;
        receiveData                 : out std_logic_vector (7 downto 0);
        receiveDataControlFlag      : out std_logic;
        receiveFIFOCount            : in  std_logic_vector(5 downto 0);
        -- serial i/o.
        spaceWireDataOut            : out std_logic;
        spaceWireStrobeOut          : out std_logic;
        spaceWireDataIn             : in  std_logic;
        spaceWireStrobeIn           : in  std_logic;
        statisticalInformationClear : in  std_logic;
        statisticalInformation      : out bit32X8Array
        );
end SpaceWireCODECIPLinkInterface;


architecture Behavioral of SpaceWireCODECIPLinkInterface is


    component SpaceWireCODECIPReceiverSynchronize is
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
            receiveFIFOWriteEnable  : out std_logic;
            enableReceive           : in  std_logic;
            spaceWireReset          : in  std_logic;
            receiveClock            : in  std_logic
            );
    end component;


    component SpaceWireCODECIPTransmitter
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
            transmitEOPAsynchronous  : out std_logic;
            transmitByteAsynchronous : out std_logic
            );
    end component;

    component SpaceWireCODECIPStateMachine
        port (
            clock                         : in  std_logic;
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
            gotTimeCode                   : in  std_logic;
            gotFCT                        : in  std_logic;
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
    end component;

    component SpaceWireCODECIPTimer is
        generic (
            gTimer6p4usValue  : integer := 640;
            gTimer12p8usValue : integer := 1280
            );
        port (
            clock            : in  std_logic;
            reset            : in  std_logic;
            timer6p4usReset  : in  std_logic;
            timer12p8usStart : in  std_logic;
            after6p4us       : out std_logic;
            after12p8us      : out std_logic
            );
    end component;


    component SpaceWireCODECIPStatisticalInformationCount is
        port (
            clock                       : in  std_logic;
            reset                       : in  std_logic;
            transmitClock               : in  std_logic;
            receiveClock                : in  std_logic;
            receiveEEPAsynchronous      : in  std_logic;
            receiveEOPAsynchronous      : in  std_logic;
            receiveByteAsynchronous     : in  std_logic;
            transmitEEPAsynchronous     : in  std_logic;
            transmitEOPAsynchronous     : in  std_logic;
            transmitByteAsynchronous    : in  std_logic;
            linkUpTransition            : in  std_logic;
            linkDownTransition          : in  std_logic;
            linkUpEnable                : in  std_logic;
            nullSynchronous             : in  std_logic;
            fctSynchronous              : in  std_logic;
            statisticalInformationClear : in  std_logic;
            statisticalInformation      : out bit32X8Array;
            characterMonitor            : out std_logic_vector(6 downto 0)
            );
    end component;



    component SpaceWireCODECIPTimeCodeControl is
        port (
            clock              : in  std_logic;
            reset              : in  std_logic;
            receiveClock       : in  std_logic;
            gotTimeCode        : in  std_logic;
            receiveTimeCodeOut : in  std_logic_vector(7 downto 0);
            timeOut            : out std_logic_vector(5 downto 0);
            controlFlagsOut    : out std_logic_vector(1 downto 0);
            tickOut            : out std_logic
            );
    end component;

    signal gotFCT                        : std_logic;
    signal gotTimeCode                   : std_logic;
    signal gotNCharacter                 : std_logic;
    signal gotNull                       : std_logic;
    signal iGotBit                       : std_logic;
    signal iCreditError                  : std_logic;
    signal parityError                   : std_logic;
    signal escapeError                   : std_logic;
    signal disconnectError               : std_logic;
    signal receiveError                  : std_logic;
    signal enableReceive                 : std_logic;
    signal sendNCharactors               : std_logic;
    signal sendTimeCode                  : std_logic;
    signal after12p8us                   : std_logic;
    signal after6p4us                    : std_logic;
    signal enableTransmit                : std_logic;
    signal sendNulls                     : std_logic;
    signal sendFCTs                      : std_logic;
    signal spaceWireResetOutSignal       : std_logic;
    signal characterSequenceError        : std_logic;
    signal timer6p4usReset               : std_logic;
    signal timer12p8usStart              : std_logic;
    signal receiveFIFOWriteEnable0       : std_logic;
    signal iReceiveFIFOWriteEnable1      : std_logic;
    signal receiveOff                    : std_logic;
    signal receiveTimeCodeOut            : std_logic_vector(7 downto 0) := x"00";
    signal linkUpTransitionSynchronize   : std_logic;
    signal linkDownTransitionSynchronize : std_logic;
    signal linkUpEnable                  : std_logic;
    signal nullSynchronize               : std_logic;
    signal fctSynchronize                : std_logic;
    signal receiveEEPAsynchronous        : std_logic;
    signal receiveEOPAsynchronous        : std_logic;
    signal receiveByteAsynchronous       : std_logic;
    signal transmitEEPAsynchronous       : std_logic;
    signal transmitEOPAsynchronous       : std_logic;
    signal transmitByteAsynchronous      : std_logic;
    signal characterMonitor              : std_logic_vector(6 downto 0);

begin


    spaceWireReceiver : SpaceWireCODECIPReceiverSynchronize
        generic map (
            gDisconnectCountValue => gDisconnectCountValue
            )
        port map (
            receiveClock               => receiveClock,
            spaceWireDataIn            => spaceWireDataIn,
            spaceWireStrobeIn          => spaceWireStrobeIn,
            receiveDataOut(7 downto 0) => receiveData,
            receiveDataOut(8)          => receiveDataControlFlag,
            receiveDataValidOut        => receiveByteAsynchronous,
            receiveTimeCodeOut         => receiveTimeCodeOut,
            receiveFIFOWriteEnable     => receiveFIFOWriteEnable0,
            receiveFCTOut              => gotFCT,
            receiveTimeCodeValidOut    => gotTimeCode,
            receiveNCharacterOut       => gotNCharacter,
            receiveNullOut             => gotNull,
            receiveEEPOut              => receiveEEPAsynchronous,
            receiveEOPOut              => receiveEOPAsynchronous,
            receiveOffOut              => receiveOff,
            receiverErrorOut           => receiveError,
            parityErrorOut             => parityError,
            escapeErrorOut             => escapeError,
            disconnectErrorOut         => disconnectError,
            enableReceive              => enableReceive,
            spaceWireReset             => spaceWireResetOutSignal
            );



    spaceWireTransmitter : SpaceWireCODECIPTransmitter
        generic map (
            gInitializeTransmitClockDivideValue => gTransmitClockDivideValue
            )
        port map (
            transmitClock            => transmitClock,
            clock                    => clock,
            receiveClock             => receiveClock,
            reset                    => reset,
            spaceWireDataOut         => spaceWireDataOut,
            spaceWireStrobeOut       => spaceWireStrobeOut,
            tickIn                   => tickIn,
            timeIn                   => timeIn,
            controlFlagsIn           => controlFlagsIn,
            transmitDataEnable       => transmitDataEnable,
            transmitData             => transmitData,
            transmitDataControlFlag  => transmitDataControlFlag,
            transmitReady            => transmitReady,
            enableTransmit           => enableTransmit,
            --autoStart.     
            sendNulls                => sendNulls,
            sendFCTs                 => sendFCTs,
            sendNCharacters          => sendNCharactors,
            sendTimeCodes            => sendTimeCode,
            --tx_fct.
            gotFCT                   => gotFCT,
            gotNCharacter            => gotNCharacter,
            receiveFIFOCount         => receiveFIFOCount,
            creditError              => iCreditError,
            transmitClockDivide      => transmitClockDivideValue,
            creditCountOut           => creditCount,
            outstandingCountOut      => outstndingCount,
            spaceWireResetOut        => spaceWireResetOutSignal,
            transmitEEPAsynchronous  => transmitEEPAsynchronous,
            transmitEOPAsynchronous  => transmitEOPAsynchronous,
            transmitByteAsynchronous => transmitByteAsynchronous
            );


    spaceWireStateMachine : SpaceWireCODECIPStateMachine
        port map (
            clock                         => clock,
            receiveClock                  => receiveClock,
            reset                         => reset,
            after12p8us                   => after12p8us,
            after6p4us                    => after6p4us,
            linkStart                     => linkStart,
            linkDisable                   => linkDisable,
            autoStart                     => autoStart,
            enableTransmit                => enableTransmit,
            sendNulls                     => sendNulls,
            sendFCTs                      => sendFCTs,
            sendNCharacter                => sendNCharactors,
            sendTimeCodes                 => sendTimeCode,
            gotFCT                        => gotFCT,
            gotTimeCode                   => gotTimeCode,
            gotNCharacter                 => gotNCharacter,
            gotNull                       => gotNull,
            gotBit                        => iGotBit,
            creditError                   => iCreditError,
            receiveError                  => receiveError,
            enableReceive                 => enableReceive,
            characterSequenceError        => characterSequenceError,
            spaceWireResetOut             => spaceWireResetOutSignal,
            FIFOAvailable                 => FIFOAvailable,
            timer6p4usReset               => timer6p4usReset,
            timer12p8usStart              => timer12p8usStart,
            linkUpTransitionSynchronize   => linkUpTransitionSynchronize,
            linkDownTransitionSynchronize => linkDownTransitionSynchronize,
            linkUpEnable                  => linkUpEnable,
            nullSynchronize               => nullSynchronize,
            fctSynchronize                => fctSynchronize
            );

    spaceWireTimer : SpaceWireCODECIPTimer
        generic map (
            gTimer6p4usValue  => gTimer6p4usValue,
            gTimer12p8usValue => gTimer12p8usValue
            )
        port map (
            clock            => clock,
            reset            => reset,
            timer6p4usReset  => timer6p4usReset,
            timer12p8usStart => timer12p8usStart,
            after6p4us       => after6p4us,
            after12p8us      => after12p8us
            );

    spaceWireStatisticalInformationCount : SpaceWireCODECIPStatisticalInformationCount
        port map (
            clock                       => clock,
            reset                       => reset,
            statisticalInformationClear => statisticalInformationClear,
            transmitClock               => transmitClock,
            receiveClock                => receiveClock,
            receiveEEPAsynchronous      => receiveEEPAsynchronous,
            receiveEOPAsynchronous      => receiveEOPAsynchronous,
            receiveByteASynchronous     => receiveByteAsynchronous,
            transmitEEPAsynchronous     => transmitEEPAsynchronous,
            transmitEOPAsynchronous     => transmitEOPAsynchronous,
            transmitByteAsynchronous    => transmitByteAsynchronous,
            linkUpTransition            => linkUpTransitionSynchronize,
            linkDownTransition          => linkDownTransitionSynchronize,
            linkUpEnable                => linkUpEnable,
            nullSynchronous             => nullSynchronize,
            fctSynchronous              => fctSynchronize,
            statisticalInformation      => statisticalInformation,
            characterMonitor            => characterMonitor
            );


    SpaceWireTimeCodeControl : SpaceWireCODECIPTimeCodeControl
        port map (
            clock              => clock,
            reset              => reset,
            receiveClock       => receiveClock,
            gotTimeCode        => gotTimeCode,
            receiveTimeCodeOut => receiveTimeCodeOut,
            timeOut            => timeOut,
            controlFlagsOut    => controlFlagsOut,
            tickOut            => tickOut
            );          


    receiveFIFOWriteEnable1  <= iReceiveFIFOWriteEnable1;
    iReceiveFIFOWriteEnable1 <= (receiveFIFOWriteEnable0 and sendNCharactors);
    iGotBit                  <= not receiveOff;
    spaceWireResetOut        <= spaceWireResetOutSignal;

----------------------------------------------------------------------
-- Define status signal as LinkStatus or ErrorStatus.
----------------------------------------------------------------------
    linkStatus (0)           <= enableTransmit;
    linkStatus (1)           <= enableReceive;
    linkStatus (2)           <= sendNulls;
    linkStatus (3)           <= sendFCTs;
    linkStatus (4)           <= sendNCharactors;
    linkStatus (5)           <= sendTimeCode;
    linkStatus (6)           <= '0';
    linkStatus (7)           <= spaceWireResetOutSignal;
    linkStatus (15 downto 8) <= "0" & characterMonitor;

    errorStatus (0) <= characterSequenceError;  --sequence.
    errorStatus (1) <= iCreditError;            --credit.
    errorStatus (2) <= receiveError;            --receiveError(=parity, discon or escape error)
    errorStatus (3) <= '0';
    errorStatus (4) <= parityError;             -- parity.
    errorStatus (5) <= disconnectError;         -- disconnect.
    errorStatus (6) <= escapeError;             -- escape.
    errorStatus (7) <= '0';

end Behavioral;
