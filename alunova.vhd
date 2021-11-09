library ieee;
use ieee.numeric_bit.all;

entity mux2to1 is
    Port ( SEL : in  bit;
           A   : in  bit;
           B   : in  bit;
           X   : out bit);
end mux2to1;

architecture Behavioral of mux2to1 is
begin
    X <= A when (SEL = '0') else B;
end Behavioral;

entity mux4to1 is
    Port ( SEL : in  bit_vector(1 downto 0);
           A   : in  bit;
           B   : in  bit;
           C   : in  bit;
           D   : in  bit;
           X   : out bit);
end mux4to1;

architecture Behavioral of mux_4to1 is
begin
    with SEL select X <=
        A when "00",
        B when "01",
        C when "10",
        D when "11";
end Behavioral;

entity fulladder is
    Port ( a, b, cin : in bit;
           s, cout : out bit);
    end fulladder;
    
    architecture gate_level of fulladder is
    begin
    
    s <= a XOR b XOR cin ;
    cout <= (a AND b) OR (cin AND a) OR (cin AND b) ;
    
    end gate_level;


entity alu1bit is 
    port (
        a, b, less, cin: in bit;
        result, cout, set, overflow: out bit;
        ainvert, binvert: in bit;
        operation: in bit_vector(1 downto 0)
    );
end entity alu1bit;

architecture behavior of alu1bit is
    signal muxA, muxB: bit;
    signal AmaisB, AmenosB, somaOut: bit;
    signal notA, notB: bit;
    signal AandB, AorB: bit;

    component mux2to1 is
        port ( SEL, A, B : in  bit;
               X : out bit);
    end component;

    component mux4to1 is
        port (SEL : in  bit_vector(1 downto 0);
              A, B, C, D : in bit;
              X : out bit);
    end component;

    component fulladder is
        Port ( a, b, cin : in bit;
               s, cout : out bit);
    end component;

begin
    notA <= not a;
    notB <= not b;

    Ainv : mux2to1 port map(ainvert, a, notA, muxA);
    Binv : mux2to1 port map(binvert, b, notB, muxB);

    AandB <= muxA and muxB;
    AorB <= muxA or muxB;

    maisAB : fulladder port map(muxA, muxB, cin, AmaisB, somaOut);

    MUX4 : mux4to1 port map(operation, AandB, AorB, AmaisB, b, result);

    set <= AmaisB;
    cout <= somaOut;
    overflow <= '1' when muxA = '1' and muxB = '1' and AmaisB = '0' else
                '1' when muxA = '0' and muxB = '0' and AmaisB = '1' else
                '0';

end behavior;


library ieee;
use ieee.numeric_bit.all;
entity alu is
    generic (
    size : natural := 10 -- bitsize
    ) ;
    port (
    A, B : in bit_vector (size - 1 downto 0) ; -- iputs
    F : out bit_vector (size - 1 downto 0) ; -- output
    S : in bit_vector (3 downto 0) ; -- opselection
    Z : out bit ; -- zeroflag
    Ov : out bit ; --overflowflag
    Co : out bit -- carryout  
    ) ;
end entity alu ;

architecture arch_alu of alu is
    component alu1bit is
        port (
            a, b, less, cin : in bit ;
            result, cout, set, overflow : out bit;
            ainvert , binvert : in bit;
            operation: in bit_vector (1 downto 0)
        ) ;
    end component;
    signal Overflow, soma, maior, Zero, Cout, menores, sets, final : bit_vector (size-1 downto 0);
    signal isSub: bit;
    begin
        isSub <= S(3) or S(2);
        menores(0) <= sets(size-1);
        alus: for i in size-1 downto 0 generate

            ula_final: if i = size-1 generate 
            ulafinal : alu1bit port map (A(i), B(i), menores(i), Cout(i-1), final(i), Cout(i), sets(i), Overflow(i), S(3), S(2), S(1 downto 0) );
            end generate;
            ula_meio: if (i > 0 and i < size - 1) generate
            ulameio: alu1bit port map (A(i), B(i), menores(i), Cout(i-1), final(i), Cout(i), sets(i), Overflow(i), S(3), S(2), S(1 downto 0) );
            end generate;
            ula_primeira: if i = 0 generate 
            ulaprimeira: alu1bit port map(A(i), B(i), menores(i), isSub, final(i), Cout(i), sets(i), Overflow(i), S(3), S(2), S(1 downto 0) );
            end generate;
        end generate;

        Zero(0) <= final(0);
        zeross: for i in 1 to size-1 generate
            Zero(i) <= Zero(i-1) or final(i);
        end generate;

        with Overflow(size - 1) select
        Ov <= '1' when '1',
              '0' when '0';
        Co <= Cout(size-1);
        maior(0) <= soma(size - 1)  when  S = "0111" else
                    '0';
        F <= final;

        Z <= not Zero(size-1);
        
end arch_alu;

