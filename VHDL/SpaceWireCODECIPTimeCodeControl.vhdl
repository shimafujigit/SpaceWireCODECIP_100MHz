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

entity SpaceWireCODECIPTimeCodeControl is

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

end SpaceWireCODECIPTimeCodeControl;

architecture Behavioral of SpaceWireCODECIPTimeCodeControl is


    component SpaceWireCODECIPSynchronizeOnePulse is
        port (
            clock             : in  std_logic;
            asynchronousClock : in  std_logic;
            reset             : in  std_logic;
            asynchronousIn    : in  std_logic;
            synchronizedOut   : out std_logic
            );
    end component;

    signal iReceiveTimeCodeOutRegister : std_logic_vector (7 downto 0);
    signal iControlFlags               : std_logic_vector(1 downto 0);
    signal iReceiveTimeCode            : std_logic_vector(5 downto 0);
    signal iReceiveTimeCodePlus1       : std_logic_vector(5 downto 0);
    signal iTickOutSignal              : std_logic;
    signal gotTimeCodeSynchronized     : std_logic;

begin

    timeOut         <= iReceiveTimeCode;
    controlFlagsOut <= iControlFlags;
    tickOut         <= iTickOutSignal;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C 8.12 System time distribution (normative)
-- ECSS-E-ST-50-12C 7.3 Control characters and control codes
-- The new time should be one more than the time-counter's previous
-- time-value.
----------------------------------------------------------------------
    process (clock, reset)
    begin
        if (reset = '1') then
            iReceiveTimeCode            <= (others => '0');
            iReceiveTimeCodePlus1       <= "000001";
            iTickOutSignal              <= '0';
            iControlFlags               <= "00";
            iReceiveTimeCodeOutRegister <= (others => '0');
        else
            if (clock'event and clock = '1') then
                if (gotTimeCodeSynchronized = '1') then
                    iControlFlags         <= iReceiveTimeCodeOutRegister (7 downto 6);
                    iReceiveTimeCode      <= iReceiveTimeCodeOutRegister (5 downto 0);
                    iReceiveTimeCodePlus1 <= iReceiveTimeCodeOutRegister (5 downto 0) + 1;
                    if (iReceiveTimeCodePlus1 = iReceiveTimeCodeOutRegister (5 downto 0)) then
                        iTickOutSignal <= '1';
                    end if;
                else
                    iTickOutSignal <= '0';
                end if;
                iReceiveTimeCodeOutRegister <= receiveTimeCodeOut;
            end if;
        end if;
    end process;

    timeCodePulse : SpaceWireCODECIPSynchronizeOnePulse
        port map (
            clock             => clock,
            asynchronousClock => receiveClock,
            reset             => reset,
            asynchronousIn    => gotTimeCode,
            synchronizedOut   => gotTimeCodeSynchronized
            );

end Behavioral;


