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

entity SpaceWireCODECIPStatisticalInformationCount is
    port (
        clock                       : in  std_logic;
        reset                       : in  std_logic;
        statisticalInformationClear : in  std_logic;
        transmitClock               : in  std_logic;
        receiveClock                : in  std_logic;
--                
        receiveEEPAsynchronous      : in  std_logic;
        receiveEOPAsynchronous      : in  std_logic;
        receiveByteAsynchronous     : in  std_logic;
--
        transmitEEPAsynchronous     : in  std_logic;
        transmitEOPAsynchronous     : in  std_logic;
        transmitByteAsynchronous    : in  std_logic;
--                
        linkUpTransition            : in  std_logic;
        linkDownTransition          : in  std_logic;
        linkUpEnable                : in  std_logic;
--                
        nullSynchronous             : in  std_logic;
        fctSynchronous              : in  std_logic;
--                
        statisticalInformation      : out bit32X8Array;
        characterMonitor            : out std_logic_vector(6 downto 0)
        );

end SpaceWireCODECIPStatisticalInformationCount;

architecture behavioral of SpaceWireCODECIPStatisticalInformationCount is

    component SpaceWireCODECIPSynchronizeOnePulse is
        port (
            clock             : in  std_logic;
            asynchronousClock : in  std_logic;
            reset             : in  std_logic;
            asynchronousIn    : in  std_logic;
            synchronizedOut   : out std_logic
            );
    end component;

    signal iTransmitEOPCount        : std_logic_vector (31 downto 0);
    signal iReceiveEOPCount         : std_logic_vector (31 downto 0);
    signal iTransmitEEPCount        : std_logic_vector (31 downto 0);
    signal iReceiveEEPCount         : std_logic_vector (31 downto 0);
    signal iTransmitByteCount       : std_logic_vector (31 downto 0);
    signal iReceiveByteCount        : std_logic_vector (31 downto 0);
    signal iLinkUpCount             : std_logic_vector (31 downto 0);
    signal iLinkDownCount           : std_logic_vector (31 downto 0);
--      
    signal iCharacterMonitor        : std_logic_vector (6 downto 0);
--
    signal iReceiveEEPSynchronize   : std_logic;
    signal iReceiveEOPSynchronize   : std_logic;
    signal iReceiveByteSynchronize  : std_logic;
    signal iTransmitEEPSynchronize  : std_logic;
    signal iTransmitEOPSynchronize  : std_logic;
    signal iTransmitByteSynchronize : std_logic;
    
begin

    characterMonitor          <= iCharacterMonitor;
    statisticalInformation(0) <= iTransmitEOPCount;
    statisticalInformation(1) <= iReceiveEOPCount;
    statisticalInformation(2) <= iTransmitEEPCount;
    statisticalInformation(3) <= iReceiveEEPCount;
    statisticalInformation(4) <= iTransmitByteCount;
    statisticalInformation(5) <= iReceiveByteCount;
    statisticalInformation(6) <= iLinkUpCount;
    statisticalInformation(7) <= iLinkDownCount;

----------------------------------------------------------------------
-- One Shot Status Information.
----------------------------------------------------------------------
    process (clock, reset)
    begin
        if (reset = '1') then
            iCharacterMonitor <= (others => '0');
        else
            iCharacterMonitor <= iReceiveEEPSynchronize & iReceiveEOPSynchronize & fctSynchronous
                                 & nullSynchronous & iReceiveByteSynchronize & iTransmitByteSynchronize & linkUpEnable;
        end if;
    end process;

----------------------------------------------------------------------
-- Statistical Information Counter.
-- Transmit and Receive EOP, EEP, 1Byte, SpaceWireLinkUP and SpaceWireLinkDown 
-- Increment Counter.
-- Status Information
-- Receive EOP, EEP, FCT, Null and 1Byte One Shot Pulse
-- Transmit 1Byte One Shot Pulse.
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Transmit EOP, EEP and 1Byte Increment Counter.
----------------------------------------------------------------------
    process (clock, reset, statisticalInformationClear)
    begin
        if (reset = '1' or statisticalInformationClear = '1') then
            iTransmitEOPCount  <= (others => '0');
            iTransmitEEPCount  <= (others => '0');
            iTransmitByteCount <= (others => '0');
        else
            if (clock'event and clock = '1') then
                if (iTransmitEEPSynchronize = '1') then
                    iTransmitEEPCount <= iTransmitEEPCount + '1';
                end if;
                if (iTransmitEOPSynchronize = '1') then
                    iTransmitEOPCount <= iTransmitEOPCount + '1';
                end if;
                if (iTransmitByteSynchronize = '1') then
                    iTransmitByteCount <= iTransmitByteCount + '1';
                end if;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- receive EOP,EEP,1Byte Increment Counter.
----------------------------------------------------------------------
    process (clock, reset, statisticalInformationClear)
    begin
        if (reset = '1' or statisticalInformationClear = '1') then
            iReceiveEOPCount  <= (others => '0');
            iReceiveEEPCount  <= (others => '0');
            iReceiveByteCount <= (others => '0');
        else
            if (clock'event and clock = '1') then
                if (iReceiveEEPSynchronize = '1') then
                    iReceiveEEPCount <= iReceiveEEPCount + '1';
                end if;
                if (iReceiveEOPSynchronize = '1') then
                    iReceiveEOPCount <= iReceiveEOPCount + '1';
                end if;
                if (iReceiveByteSynchronize = '1') then
                    iReceiveByteCount <= iReceiveByteCount +'1';
                end if;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- SpaceWireLinkUP and SpaceWireLinkDown Increment Counter.
----------------------------------------------------------------------
    process (clock, reset, statisticalInformationClear)
    begin
        if (reset = '1' or statisticalInformationClear = '1') then
            iLinkUpCount   <= (others => '0');
            iLinkDownCount <= (others => '0');
        else
            if (clock'event and clock = '1') then
                if (linkUpTransition = '1') then
                    iLinkUpCount <= iLinkUpCount + '1';
                end if;
                if (linkDownTransition = '1') then
                    iLinkDownCount <= iLinkDownCount + '1';
                end if;
            end if;
        end if;
    end process;

-------------------------------------------------------------
-------------------------------------------------------------
-------------------------------------------------------------    
    receiveEEPPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => receiveEEPAsynchronous,
            synchronizedOut   => iReceiveEEPSynchronize
            );   

    receiveEOPPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => receiveEOPAsynchronous,
            synchronizedOut   => iReceiveEOPSynchronize
            );  

    receiveBytePulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => receiveByteAsynchronous,
            synchronizedOut   => iReceiveByteSynchronize
            );  

    transmitEEPPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => transmitClock,
            reset             => reset,
            asynchronousIn    => transmitEEPAsynchronous,
            synchronizedOut   => iTransmitEEPSynchronize
            );  

    transmitEOPPulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => transmitClock,
            reset             => reset,
            asynchronousIn    => transmitEOPAsynchronous,
            synchronizedOut   => iTransmitEOPSynchronize
            );  

    transmitBytePulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => transmitClock,
            reset             => reset,
            asynchronousIn    => transmitByteAsynchronous,
            synchronizedOut   => iTransmitByteSynchronize
            );                                  

end behavioral;
