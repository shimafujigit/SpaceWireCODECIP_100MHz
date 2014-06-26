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

entity SpaceWireCODECIPSynchronizeOnePulse is
    port (
        clock             : in  std_logic;
        asynchronousClock : in  std_logic;
        reset             : in  std_logic;
        asynchronousIn    : in  std_logic;
        synchronizedOut   : out std_logic
        );
end SpaceWireCODECIPSynchronizeOnePulse;

architecture Behavioral of SpaceWireCODECIPSynchronizeOnePulse is

    signal iLatchedAsynchronous : std_logic;
    signal iSynchronousRegister : std_logic;
    signal iSynchronousClear    : std_logic;
    signal iSynchronizedOut     : std_logic;

begin

----------------------------------------------------------------------
-- Synchronize the asynchronous One Shot Pulse to Clock.
----------------------------------------------------------------------

    synchronizedOut <= iSynchronizedOut;
----------------------------------------------------------------------
-- latch the rising edge of the input signal.
----------------------------------------------------------------------
    process (asynchronousIn, reset, iSynchronousClear)
    begin
        if (reset = '1' or iSynchronousClear = '1') then
            iLatchedAsynchronous <= '0';
        elsif (asynchronousIn'event and asynchronousIn = '1') then
            iLatchedAsynchronous <= '1';
        end if;
    end process;

----------------------------------------------------------------------
-- Synchronize a latch signal to Clock.
----------------------------------------------------------------------
    process (clock, reset, iSynchronousClear)
    begin
        if (reset = '1' or iSynchronousClear = '1') then
            iSynchronousRegister <= '0';
        elsif (clock'event and clock = '1') then
            if (iLatchedAsynchronous = '1') then
                iSynchronousRegister <= '1';
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Output Clock synchronized One_Shot_Pulse and clear signal.
----------------------------------------------------------------------
    process (clock, reset, iSynchronousRegister)
    begin
        if (reset = '1') then
            iSynchronizedOut  <= '0';
            iSynchronousClear <= '0';
        elsif (clock'event and clock = '1') then
            if (iSynchronousRegister = '1' and iSynchronousClear = '0') then
                iSynchronizedOut  <= '1';
                iSynchronousClear <= '1';
            elsif (iSynchronousRegister = '1') then
                iSynchronizedOut  <= '0';
                iSynchronousClear <= '0';
            else
                iSynchronizedOut  <= '0';
                iSynchronousClear <= '0';
            end if;
        end if;
    end process;

end Behavioral;
