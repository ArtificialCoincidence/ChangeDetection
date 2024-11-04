library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;  
use work.types.all

entity  bitonic_sorter is
    generic (
        WORDS     :  integer :=  8;
        WORD_BITS :  integer := 64;
        COMP_HIGH :  integer := 63;
        COMP_LOW  :  integer := 32;
        INFO_BITS :  integer :=  4
    );
    port (
        CLK       :  in  std_logic;
        RST       :  in  std_logic;
        CLR       :  in  std_logic;
        I_SORT    :  in  std_logic;
        I_UP      :  in  std_logic;
        I_DATA    :  in  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
        I_INFO    :  in  std_logic_vector(      INFO_BITS-1 downto 0);
        O_SORT    :  out std_logic;
        O_UP      :  out std_logic;
        O_DATA    :  out std_logic_vector(WORDS*WORD_BITS-1 downto 0);
        O_INFO    :  out std_logic_vector(      INFO_BITS-1 downto 0)
    );
end bitonic_sorter;

architecture rtl of bitonic_sorter is

    component bitonic_sorter
        generic (
            WORDS     :  integer := 16;
            WORD_BITS :  integer := 32;
            INFO_BITS :  integer := 4
        );
        port (
            CLK       :  in  std_logic;
            RST       :  in  std_logic;
            CLR       :  in  std_logic;
            I_SORT    :  in  std_logic;
            I_UP      :  in  std_logic;
            I_DATA    :  in  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
            I_INFO    :  in  std_logic_vector(      INFO_BITS-1 downto 0);
            O_SORT    :  out std_logic;
            O_UP      :  out std_logic;
            O_DATA    :  out std_logic_vector(WORDS*WORD_BITS-1 downto 0);
            O_INFO    :  out std_logic_vector(      INFO_BITS-1 downto 0)
        );
    end component;

    component merge
        generic (
            WORDS     :  integer := 16;
            WORD_BITS :  integer := 32;
            INFO_BITS :  integer := 4
        );
        port (
            CLK       :  in  std_logic;
            RST       :  in  std_logic;
            CLR       :  in  std_logic;
            I_SORT    :  in  std_logic;
            I_UP      :  in  std_logic;
            I_DATA    :  in  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
            I_INFO    :  in  std_logic_vector(      INFO_BITS-1 downto 0);
            O_SORT    :  out std_logic;
            O_UP      :  out std_logic;
            O_DATA    :  out std_logic_vector(WORDS*WORD_BITS-1 downto 0);
            O_INFO    :  out std_logic_vector(      INFO_BITS-1 downto 0)
        );
    end component;

begin

    ONE: if (WORDS <= 1) generate
        O_DATA <= I_DATA;
        O_INFO <= I_INFO;
        O_SORT <= I_SORT;
        O_UP   <= I_UP;
    end generate;

    ANY: if (WORDS > 1) generate
        constant UP_POS :  integer := I_INFO'high+1;
        signal   s_info :  std_logic_vector(    I_INFO'high+1 downto 0);
        signal   q_info :  std_logic_vector(    I_INFO'high+1 downto 0);
        signal   q_data :  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
        signal   q_sort :  std_logic;
    begin
        s_info(UP_POS      ) <= I_UP;
        s_info(I_INFO'range) <= I_INFO;

        FIRST : bitonic_sorter generic map (WORDS/2, WORD_BITS, COMP_HIGH, COMP_LOW, s_info'length)
            port map (
                CLK     => CLK,
                RST     => RST,
                CLR     => CLR,
                I_SORT  => I_SORT,
                I_UP    => '1',
                I_INFO  => s_info,
                I_DATA  => I_DATA(WORD_BITS*(WORDS/2)-1 downto WORD_BITS*0),
                O_SORT  => q_sort,
                O_UP    => open,
                O_INFO  => q_info,
                O_DATA  => q_data(WORD_BITS*(WORDS/2)-1 downto WORD_BITS*0)
            );

        SECOND: bitonic_sorter generic map (WORDS/2, WORD_BITS, COMP_HIGH, COMP_LOW, s_info'length)
            port map (
                CLK     => CLK,
                RST     => RST,
                CLR     => CLR,
                I_SORT  => I_SORT,
                I_UP    => '0',
                I_INFO  => s_info,
                I_DATA  => I_DATA(WORD_BITS*(WORDS)-1 downto WORD_BITS*(WORDS/2)),
                O_SORT  => open,
                O_UP    => open,
                O_INFO  => open,
                O_DATA  => q_data(WORD_BITS*(WORDS)-1 downto WORD_BITS*(WORDS/2))
            );

        MERGE : merge  generic map (WORDS  , WORD_BITS, COMP_HIGH, COMP_LOW, INFO_BITS)
            port map (
                CLK     => CLK,
                RST     => RST,
                CLR     => CLR,
                I_SORT  => q_sort,
                I_UP    => q_info(UP_POS),
                I_INFO  => q_info(I_INFO'range),
                I_DATA  => q_data,
                O_SORT  => O_SORT,
                O_UP    => O_UP  ,
                O_INFO  => O_INFO,
                O_DATA  => O_DATA
            );
    end generate;
    
end rtl;