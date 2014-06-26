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

entity SpaceWireCODECIPFIFO9x64 is
    port (
        writeDataIn    : in  std_logic_vector(8 downto 0);
        readClock      : in  std_logic;
        readEnable     : in  std_logic;
        reset          : in  std_logic;
        writeClock     : in  std_logic;
        writeEnable    : in  std_logic;
        readDataOut    : out std_logic_vector(8 downto 0);
        empty          : out std_logic;
        full           : out std_logic;
        readDataCount  : out std_logic_vector(5 downto 0);
        writeDataCount : out std_logic_vector(5 downto 0)
        );

end SpaceWireCODECIPFIFO9x64;

architecture RTL of SpaceWireCODECIPFIFO9x64 is

    type   turnMemory is array(0 to 63) of std_logic_vector(8 downto 0);
    signal dpram : turnMemory;

    type turnTable is array(0 to 63) of std_logic_vector(5 downto 0);

    constant binaryToGray : turnTable := (
        "000000", "000001", "000011", "000010",
        "000110", "000111", "000101", "000100",
        "001100", "001101", "001111", "001110",
        "001010", "001011", "001001", "001000",
        "011000", "011001", "011011", "011010",
        "011110", "011111", "011101", "011100",
        "010100", "010101", "010111", "010110",
        "010010", "010011", "010001", "010000",
        "110000", "110001", "110011", "110010",
        "110110", "110111", "110101", "110100",
        "111100", "111101", "111111", "111110",
        "111010", "111011", "111001", "111000",
        "101000", "101001", "101011", "101010",
        "101110", "101111", "101101", "101100",
        "100100", "100101", "100111", "100110",
        "100010", "100011", "100001", "100000");

    constant grayToBinary : turnTable := (
        "000000", "000001", "000011", "000010",
        "000111", "000110", "000100", "000101",
        "001111", "001110", "001100", "001101",
        "001000", "001001", "001011", "001010",
        "011111", "011110", "011100", "011101",
        "011000", "011001", "011011", "011010",
        "010000", "010001", "010011", "010010",
        "010111", "010110", "010100", "010101",
        "111111", "111110", "111100", "111101",
        "111000", "111001", "111011", "111010",
        "110000", "110001", "110011", "110010",
        "110111", "110110", "110100", "110101",
        "100000", "100001", "100011", "100010",
        "100111", "100110", "100100", "100101",
        "101111", "101110", "101100", "101101",
        "101000", "101001", "101011", "101010");

    signal iWriteReset        : std_logic;
    signal iReadReset         : std_logic;
    signal iWriteResetTime    : std_logic_vector(1 downto 0);
    signal iReadResetTime     : std_logic_vector(1 downto 0);
    signal iWritePointer      : std_logic_vector(5 downto 0);
    signal iGrayWritePointer  : std_logic_vector(5 downto 0);
    signal iGrayWritePointer1 : std_logic_vector(5 downto 0);
    signal iGrayWritePointer2 : std_logic_vector(5 downto 0);
    signal iGrayWritePointer3 : std_logic_vector(5 downto 0);
    signal iWritePointer4     : std_logic_vector(5 downto 0);
    signal iReadPointer       : std_logic_vector(5 downto 0);
    signal iGrayReadPointer   : std_logic_vector(5 downto 0);
    signal iGrayReadPointer1  : std_logic_vector(5 downto 0);
    signal iGrayReadPointer2  : std_logic_vector(5 downto 0);
    signal iReadPointer3      : std_logic_vector(5 downto 0);
    signal iWriteDataCount    : std_logic_vector(5 downto 0);
    signal iFull              : std_logic;
    signal iReadDataOut       : std_logic_vector(8 downto 0);
    signal iReadDataCount     : std_logic_vector(5 downto 0);
    signal iEmpty             : std_logic;

begin

    writeDataCount <= iWriteDataCount;
    full           <= iFull;
    empty          <= iEmpty;
    readDataCount  <= iReadDataCount;
    readDataOut    <= iReadDataOut;

----------------------------------------------------------------------
-- synchronized Reset.
----------------------------------------------------------------------
    process(reset, writeClock)
    begin
        if (reset = '1') then
            iWriteResetTime <= "11";
            iWriteReset     <= '1';
        elsif (writeClock'event and writeClock = '1') then
            iWriteResetTime <= iWriteResetTime(0) & reset;
            iWriteReset     <= iWriteResetTime(1);
        end if;
    end process;

----------------------------------------------------------------------
-- Write pointer of the buffer.
----------------------------------------------------------------------
    process(iWriteReset, writeClock)
    begin
        if (writeClock'event and writeClock = '1') then
            if (iWriteReset = '1') then
                iWritePointer <= "000000";
            elsif (writeEnable = '1') then
                iWritePointer <= iWritePointer + '1';
            end if;
        end if;
    end process;

    iWriteDataCount <= iWritePointer - iReadPointer3;

----------------------------------------------------------------------
-- Writing to buffer.
----------------------------------------------------------------------
    process(writeClock)
    begin
        if (writeClock'event and writeClock = '1') then
            if (writeEnable = '1') then
                dpram(conv_integer(iWritePointer)) <= writeDataIn;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Change to Gray code.
----------------------------------------------------------------------
    process(iWriteReset, writeClock)
    begin
        if (writeClock'event and writeClock = '1') then
            if (iWriteReset = '1') then
                iGrayWritePointer <= "000000";
            else
                iGrayWritePointer <= binaryToGray(conv_integer(iWritePointer));
            end if;
        end if;
    end process;

    iFull <= '1' when ((iWritePointer - iReadPointer3) > "111000") or iWriteReset = '1' else '0';

----------------------------------------------------------------------
-- Convert gray code Readpointer to binary Readpointer to calculate writeDataCount and full.
----------------------------------------------------------------------
    process(iWriteReset, writeClock)
    begin
        if (writeClock'event and writeClock = '1') then
            if (iWriteReset = '1') then
                iGrayReadPointer1 <= "000000";
                iGrayReadPointer2 <= "000000";
                iReadPointer3     <= "000000";
            else
                iGrayReadPointer1 <= iGrayReadPointer;
                iGrayReadPointer2 <= iGrayReadPointer1;
                iReadPointer3     <= grayToBinary(conv_integer(iGrayReadPointer2));
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Convert gray code Writepointer to binary Writepointer to calculate readDataCount and empty.
----------------------------------------------------------------------
    process(iReadReset, readClock)
    begin
        if (readClock'event and readClock = '1') then
            if (iReadReset = '1') then
                iGrayWritePointer1 <= "000000";
                iGrayWritePointer2 <= "000000";
                iGrayWritePointer3 <= "000000";
                iWritePointer4     <= "000000";
            else
                iGrayWritePointer1 <= iGrayWritePointer;
                iGrayWritePointer2 <= iGrayWritePointer1;
                iGrayWritePointer3 <= iGrayWritePointer2;
                iWritePointer4     <= grayToBinary(conv_integer(iGrayWritePointer3));
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Read from buffer.
----------------------------------------------------------------------
    process(readClock)
    begin
        if (readClock'event and readClock = '1') then
            if(iEmpty = '0')then
                if (readEnable = '1') then
                    iReadDataOut <= dpram(conv_integer(iReadPointer));
                end if;
            end if;
        end if;
    end process;

    iReadDataCount <= iWritePointer4 - iReadPointer;

----------------------------------------------------------------------
-- Read pointer of the buffer.
----------------------------------------------------------------------
    process(iReadReset, readClock)
    begin
        if (readClock'event and readClock = '1') then
            if (iReadReset = '1') then
                iReadPointer <= "000000";
            elsif(iEmpty = '0')then
                if readEnable = '1' then
                    iReadPointer <= iReadPointer + '1';
                end if;
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Change to Gray code.
----------------------------------------------------------------------
    process(iReadReset, readClock)
    begin
        if (readClock'event and readClock = '1') then
            if (iReadReset = '1') then
                iGrayReadPointer <= "000000";
            else
                iGrayReadPointer <= binaryToGray(conv_integer(iReadPointer));
            end if;
        end if;
    end process;

----------------------------------------------------------------------
-- Generate the EMPTY signal.
----------------------------------------------------------------------
    iEmpty <= '1' when iWritePointer4 = iReadPointer or iReadReset = '1' else '0';

----------------------------------------------------------------------
-- synchronized Reset.
----------------------------------------------------------------------
    process(reset, readClock)
    begin
        if (reset = '1') then
            iReadResetTime <= "11";
            iReadReset     <= '1';
        elsif (readClock'event and readClock = '1') then
            iReadResetTime <= iReadResetTime(0) & reset;
            iReadReset     <= iReadResetTime(1);
        end if;
    end process;

end RTL;
