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

package SpaceWireCODECIPPackage is

--------------------------------------------------------------------------------
--  Declare constants.
--------------------------------------------------------------------------------
    constant gDisconnectCountValue               : integer range 0 to 255        := 141;       -- transmitClock period * gDisconnectCountValue = 850ns.
    constant gTimer6p4usValue                    : integer range 0 to 1023       := 320;       -- Clock period * gTimer6p4usValue = 6.4us.
    constant gTimer12p8usValue                   : integer range 0 to 2047       := 640;       -- Clock period * gTimer12p8usValue = 12.8us.
    constant gInitializeTransmitClockDivideValue : std_logic_vector (5 downto 0) := "001001";  -- transmitClock frequency / (gInitializeTransmitClockDivideValue + 1) = 10MHz.

    type bit32X8Array is array (7 downto 0) of std_logic_vector (31 downto 0);


end SpaceWireCODECIPPackage;

package body SpaceWireCODECIPPackage is

end SpaceWireCODECIPPackage;
