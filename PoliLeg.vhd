--Reg--
library ieee;
use IEEE.numeric_bit.all;

entity reg is
    generic (wordSize: natural :=4);
    port(
        clock: in bit;
        reset: in bit;
        load: in bit;
        d: in bit_vector(wordSize-1 downto 0);
        q: out bit_vector(wordSize-1 downto 0)
    );
    end reg;
    
    architecture arch_reg of reg is
        -- "variaveis e constantes"
        signal registrador : bit_vector(wordSize-1 downto 0);
    
        begin
            process(clock, reset)
            begin
                if reset = '1' then
                    registrador <= (others => '0');
                end if;
                if load = '1' then
                    if (clock'event and clock = '1') then
                        registrador <= d;
                    end if;
                end if;
            end process;
            q <= registrador;
        end arch_reg;

library ieee;
use IEEE.numeric_bit.all;
use ieee.std_logic_1164.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;
entity regfile is
    generic(
            regn: natural := 32;
            wordSize: natural := 64
    );
    port(
            clock: in bit;
            reset: in bit;
            regWrite: in bit;
            rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn))))-1 downto 0);
            d: in bit_vector(wordSize-1 downto 0);
            q1, q2: out bit_vector(wordSize-1 downto 0)
    );
end regfile;

architecture arch_regfile of regfile is
    type BancoReg is array (0 to regn-1) of bit_vector(wordSize-1 downto 0);
    signal bregs: BancoReg;
    begin
    process(clock, reset)
        begin
            if reset = '1' then 
                for i in BancoReg'range loop 
                    bregs(i) <= (others => '0');
                end loop;
            elsif regWrite = '1' then
                if (clock = '1' and clock'event) then
                    if (to_integer(unsigned(wr)) /= regn-1) then
                        bregs(to_integer(unsigned(wr))) <= d;
                    end if;
                end if;
            end if;
    end process;
    q1 <= bregs(to_integer(unsigned(rr1)));
    q2 <= bregs(to_integer(unsigned(rr2)));
end arch_regfile;      

-- ALU control--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;

entity alucontrol is
    port(
        aluop: in bit_vector(1 downto 0);
        opcode: in bit_vector(10 downto 0);
        aluCtrl: out bit_vector(3 downto 0)
    );
end entity;

architecture arch_alucontrol of alucontrol is
begin
    aluCtrl <= "0010" when aluop = "00" else
               "0111" when aluop = "01" else
               "0010" when aluop = "10" and opcode = "10001011000" else
               "0110" when aluop = "10" and opcode = "11001011000" else
               "0000" when aluop = "10" and opcode = "10001010000" else
               "0001" when aluop = "10" and opcode = "10101010000" else
               unaffected;
end arch_alucontrol;


--Control Unit--
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
--SIGN EXTEND--

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


--ULA 64--

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

library ieee;
use IEEE.numeric_bit.all;

entity mux4to1 is
    Port ( SEL : in  bit_vector(1 downto 0);
           A   : in  bit;
           B   : in  bit;
           C   : in  bit;
           D   : in  bit;
           X   : out bit);
end mux4to1;

architecture Behavioral of mux4to1 is
begin
    with SEL select X <=
        A when "00",
        B when "01",
        C when "10",
        D when "11";
end Behavioral;

library ieee;
use IEEE.numeric_bit.all;

entity fulladder is
    Port ( a, b, cin : in bit;
           s, cout : out bit);
    end fulladder;
    
    architecture gate_level of fulladder is
    begin
    
    s <= a XOR b XOR cin ;
    cout <= (a AND b) OR (cin AND a) OR (cin AND b) ;
    
    end gate_level;

    library ieee;
    use IEEE.numeric_bit.all;

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

-- Shift Left 2 --
library ieee;
use IEEE.numeric_bit.all;

entity shiftleft2 is
    port(
        i: in bit_vector(63 downto 0);
        o: out bit_vector(63 downto 0)
    );
end shiftleft2;

architecture shift2_arch of shiftleft2 is
begin
    o(63 downto 2) <= i(61 downto 0);
    o(1 downto 0) <= (others => '0');
end architecture;


-- DataPath--


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;

entity datapath is
    port(
        clock : in bit;
        reset : in bit;
        reg2loc : in bit;
        pcsrc : in bit;
        memToReg : in bit;
        aluCtrl : in bit_vector(3 downto 0);
        aluSrc : in bit;
        regWrite : in bit;
        opcode : out bit_vector(10 downto 0);
        zero : out bit;
        imAddr : out bit_vector(63 downto 0);
        imOut: in bit_vector(31 downto 0);
        dmAddr : out bit_vector(63 downto 0);
        dmIn : out bit_vector(63 downto 0);
        dmOut : in bit_vector(63 downto 0) 
    );
end datapath;

architecture arch_datapath of datapath is
    component reg is
            generic (wordSize: natural :=64);
            port(
                clock: in bit;
                reset: in bit;
                load: in bit;
                d: in bit_vector(wordSize-1 downto 0);
                q: out bit_vector(wordSize-1 downto 0)
            );
    end component;

    component shiftleft2 is
        port(
            i: in bit_vector(63 downto 0);
            o: out bit_vector(63 downto 0)
        );
    end component;
    
    component signExtend is 
        port (
            i : in  bit_vector(31 downto 0);
            o : out bit_vector(63 downto 0)
    );
    end component;

    component alu is
        generic (
        size : natural := 64
        );
        port (
            A, B : in  bit_vector (size-1 downto 0);
            F    : out bit_vector (size-1 downto 0);
            S    : in  bit_vector (3 downto 0);
            Z    : out bit;
            Ov   : out bit;
            Co   : out bit
        );
    end component;

    component regfile is 
        generic(
            regn: natural := 32;
            wordSize: natural := 64
        );
        port(
            clock:      in  bit;
            reset:      in  bit;
            regWrite:   in  bit;
            rr1,rr2,wr: in  bit_vector(4 downto 0);
            d:          in  bit_vector(wordSize-1 downto 0);
            q1, q2:     out bit_vector(wordSize-1 downto 0)
    );
    end component;
    signal rr2 : bit_vector(4 downto 0);
    signal d_reg, alu_res, sign_ext, left_shift, alu_in, reg1, reg2 : bit_vector(63 downto 0);
    signal PCin, PCout, PCalu_result, PCalu_branch : bit_vector(63 downto 0);
    signal quatro : bit_vector(63 downto 0);

begin 
    quatro(2 downto 0) <= "100";
    opcode <= imOut (31 downto 21);

    banco_registradores : regfile
    generic map(32, 64)
    port map(clock, reset, regWrite, imOut(9 downto 5), rr2, imOut(4 downto 0), d_reg, reg1, reg2);
    alu1: alu
    generic map(64)

    port map(reg1, alu_in, alu_res, aluCtrl, zero);
    sign_extend : signExtend 
    port map ( imOut, sign_ext);
    shift_left : shiftleft2
    port map (sign_ext, left_shift);
    alu_Branch : alu
    generic map(64)
    port map (PCout, left_shift, PCalu_branch, "0010");
    alu_not_Branch : alu
    generic map(64)
    port map(PCout, quatro, PCalu_result, "0010");
    PC : reg 
    port map (clock, reset, '1', PCin, PCout);
    
    PCin <= PCalu_result when pcsrc = '0' else
            PCalu_branch;
    imAddr <= PCout;
    dmIn <= reg2;
    rr2 <= imOut(20 downto 16) when reg2loc = '0' else
           imOut(4 downto 0);

    d_reg <= dmOut when memToReg = '1' else
             alu_res;
    
    alu_in <= reg2 when aluSrc = '0' else
               sign_ext;

    dmAddr <= alu_res;
    end arch_datapath;


    --POLILEG--
library ieee;
use IEEE.numeric_bit.all;

entity polilegsc is
     port(
        clock, reset: in bit;
        dmem_addr: out bit_vector(63 downto 0);
        dmem_dati: out bit_vector(63 downto 0);
        dmem_dato: in bit_vector(63 downto 0);
        dmem_we: out bit;
        imem_addr: out bit_vector(63 downto 0);
        imem_data: in bit_vector(31 downto 0)
        );
 end entity;

 architecture arch_leg of polilegsc is
    signal reg2loc, uncondBranch, branch, memRead, memToReg, memWrite, aluSrc, regWrite, zero, pcsrc : bit;
    signal aluop : bit_vector(1 downto 0);
    signal aluCtrl : bit_vector(3 downto 0);
    signal opcode : bit_vector(10 downto 0);
    signal imAddr, dmAddr, dmIn : bit_vector(63 downto 0);

    component alucontrol is
        port (
        aluop : in bit_vector(1 downto 0);
        opcode: in bit_vector(10 downto 0);
        aluCtrl: out bit_vector(3 downto 0)
    );
    end component;

    component controlunit is
        port (
        reg2loc : out bit ;
        uncondBranch : out bit ;
        branch : out bit ;
        memRead: out bit ;
        memToReg : out bit ;
        aluOp : out bit_vector(1 downto 0 ) ;
        memWrite : out bit ;
        aluSrc : out bit ;
        regWrite : out bit;
        opcode : in bit_vector(10 downto 0)
    );
    end component;

    component datapath is 
        port(
            clock: in bit;
            reset: in bit;

            reg2loc: in bit;
            pcsrc: in bit;
            memToReg: in bit;
            aluCtrl: in bit_vector(3 downto 0);
            aluSrc: in bit;
            regWrite: in bit;

            opcode: out bit_vector(10 downto 0);
            zero: out bit;

            imAddr: out bit_vector(63 downto 0);
            imOut: in bit_vector(31 downto 0);

            dmAddr: out bit_vector(63 downto 0);
            dmIn: out bit_vector(63 downto 0);
            dmOut: in bit_vector(63 downto 0)
        );
    end component;

begin
    pcsrc <= uncondBranch or (zero and branch);
    control_unit : controlunit port map (reg2loc, uncondBranch, branch, memRead, memToReg, aluOp, memWrite, aluSrc, regWrite, opcode );
    alu_control : alucontrol port map (aluOp, opcode, aluCtrl);
    data_path : datapath port map (clock, reset, reg2loc, pcsrc, memToReg, aluCtrl, aluSrc, regWrite, opcode, zero, imAddr, imem_data, dmAddr, dmIn, dmem_dato);

dmem_addr <= dmAddr;
dmem_dati <= dmIn;
dmem_we <= memWrite;

imem_addr <= imAddr;

end arch_leg;





    




    
