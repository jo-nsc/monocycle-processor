library ieee;
use ieee.numeric_bit.all;


entity signExtend is
    port(
        i: in bit_vector(31 downto 0);
        o: out bit_vector(63 downto 0)
    );
end signExtend;

architecture arch_signExtend of signExtend is
    signal adress_out : bit_vector(63 downto 0);
    begin
        adress_out <= bit_vector(resize(signed(i(20 downto 12)), o'length)) when i(31 downto 24) = "11111000" else
                      bit_vector(resize(signed(i(23 downto 5)), o'length)) when i(31 downto 24) = "10110100" else
                      bit_vector(resize(signed(i(25 downto 0)), o'length)) when i(31 downto 26) = "000101" else
                      unaffected;
        o <= adress_out;
end arch_signExtend;