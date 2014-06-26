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

entity SpaceWireCODECIPTimer is
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
end SpaceWireCODECIPTimer;

architecture Behavioral of SpaceWireCODECIPTimer is

    signal iTimerState12p8us : std_logic;
    signal iTimerCount6p4us  : std_logic_vector (9 downto 0);
    signal iTimerCount12p8us : std_logic_vector (10 downto 0);
    signal iAfter6p4us       : std_logic;
    signal iAfter12p8us      : std_logic;


begin

    after6p4us  <= iAfter6p4us;
    after12p8us <= iAfter12p8us;

----------------------------------------------------------------------
-- ECSS-E-ST-50-12C  8.4.7 Timer.
-- The timer provides the After 6.4 us and After 12.8 us timeouts used 
-- in link initialization.
----------------------------------------------------------------------

----------------------------------------------------------------------
-- After 6.4us.
----------------------------------------------------------------------
    controlTimer64 : process (clock, reset, timer6p4usReset)
    begin
        if (reset = '1' or timer6p4usReset = '1') then
            iTimerCount6p4us <= (others => '0');
            iAfter6p4us      <= '0';
        elsif (clock'event and clock = '1') then
            if (iTimerCount6p4us < gTimer6p4usValue) then
                iTimerCount6p4us <= iTimerCount6p4us + 1;
                iAfter6p4us      <= '0';
            else
                iTimerCount6p4us <= (others => '0');
                iAfter6p4us      <= '1';
            end if;
        end if;

    end process;

----------------------------------------------------------------------
-- After 12.8us.
----------------------------------------------------------------------
    controlTimer128 : process (clock, reset, timer12p8usStart, timer6p4usReset)
    begin
        if (reset = '1' or timer6p4usReset = '1') then
            iTimerState12p8us <= '0';
            iTimerCount12p8us <= (others => '0');
            iAfter12p8us      <= '0';
        elsif (clock'event and clock = '1') then
            if (iTimerState12p8us = '0') then
                iAfter12p8us <= '0';
                if (timer12p8usStart = '1') then
                    iTimerState12p8us <= '1';
                end if;
            else
                if (iTimerCount12p8us < gTimer12p8usValue) then
                    iTimerCount12p8us <= iTimerCount12p8us + 1;
                    iAfter12p8us      <= '0';
                else
                    iTimerCount12p8us <= (others => '0');
                    iTimerState12p8us <= '0';
                    iAfter12p8us      <= '1';
                end if;
            end if;
        end if;
    end process;
    
end Behavioral;
