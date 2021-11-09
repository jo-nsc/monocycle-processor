library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;
entity controlunit is
    port(
        reg2loc: out bit;
        uncondBranch : out bit;
        branch: out bit;
        memRead: out bit;
        memToReg: out bit;
        aluOp: out bit_vector(1 downto 0);
        memWrite: out bit;
        aluSrc: out bit;
        regWrite: out bit;
        opcode: in bit_vector(10 downto 0)
    );
end entity;

architecture arch_controlunit of controlunit is
    signal sinais : bit_vector(9 downto 0);
    begin
        sinais <= "0001100011" when opcode = "11111000010" else
                  "1000000110" when opcode = "11111000000" else
                  "1010001000" when opcode(10 downto 3) = "10110100" else
                  "0100001000" when opcode(10 downto 5) = "000101" else
                  "0000010001" when opcode = "10001011000" or opcode = "11001011000" or opcode = "10001010000" or opcode = "10101010000" else
                  unaffected;
        reg2loc <= sinais(9);
        uncondBranch <= sinais(8);
        branch <= sinais(7);
        memRead <= sinais (6);
        memToReg <= sinais(5);
        aluOp <= sinais(4 downto 3);
        memWrite <= sinais (2);
        aluSrc <= sinais(1);
        regWrite <= sinais(0);
    end arch_controlunit;