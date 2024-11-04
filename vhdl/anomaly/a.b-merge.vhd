library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;  
use work.types.all; 

entity merge is
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
end merge;

architecture rtl of merge is

    component merge
        generic (
            WORDS     :  integer := 16;
            WORD_BITS :  integer := 32;
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
    end component;

    component exchange
        generic (
            WORD_BITS :  integer := 32;
        );
        port (
            I_SORT    :  in  std_logic;
            I_UP      :  in  std_logic;
            I_A       :  in  ufixed(-1 downto -WORD_BITS);
            I_B       :  in  ufixed(-1 downto -WORD_BITS);
            O_A       :  out ufixed(-1 downto -WORD_BITS);
            O_B       :  out ufixed(-1 downto -WORD_BITS)
        );
    end component;

begin

    ONE: if (WORDS = 1) generate
        O_DATA <= I_DATA;
        O_INFO <= I_INFO;
        O_SORT <= I_SORT;
        O_UP   <= I_UP;
    end generate;

    ANY: if (WORDS > 1) generate
        constant DIST   :  integer := WORDS / 2;
        signal   s_data :  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
        signal   q_data :  std_logic_vector(WORDS*WORD_BITS-1 downto 0);
        signal   q_info :  std_logic_vector(      INFO_BITS-1 downto 0);
        signal   q_sort :  std_logic;
        signal   q_up   :  std_logic;
    begin
        XCHG: for i in 0 to DIST-1 generate
            U: exchange generic map(WORD_BITS, COMP_HIGH, COMP_LOW)
            port map (
                I_SORT  => I_SORT,
                I_UP    => I_UP  ,
                I_A     => I_DATA(WORD_BITS*(i     +1)-1 downto WORD_BITS*(i     )),
                I_B     => I_DATA(WORD_BITS*(i+DIST+1)-1 downto WORD_BITS*(i+DIST)),
                O_A     => s_data(WORD_BITS*(i     +1)-1 downto WORD_BITS*(i     )),
                O_B     => s_data(WORD_BITS*(i+DIST+1)-1 downto WORD_BITS*(i+DIST))
            );
        end generate;

        process (CLK, RST) begin
            if (RST = '1') then
                    q_data <= (others => '0');
                    q_info <= (others => '0');
                    q_sort <= '1';
                    q_up   <= '1';
            elsif (CLK'event and CLK = '1') then
                if (CLR = '1') then
                    q_data <= (others => '0');
                    q_info <= (others => '0');
                    q_sort <= '1';
                    q_up   <= '1';
                else
                    q_data <= s_data;
                    q_info <= I_INFO;
                    q_sort <= I_SORT;
                    q_up   <= I_UP;
                end if;
            end if;
        end process;

        FIRST : merge generic map (WORDS/2, WORD_BITS, COMP_HIGH, COMP_LOW, INFO_BITS)
            port map (
                CLK     => CLK,
                RST     => RST,
                CLR     => CLR,
                I_SORT  => q_sort,
                I_UP    => q_up,
                I_INFO  => q_info,
                I_DATA  => q_data(WORD_BITS*(WORDS/2)-1 downto WORD_BITS*0),
                O_SORT  => O_SORT,
                O_UP    => O_UP,
                O_INFO  => O_INFO,
                O_DATA  => O_DATA(WORD_BITS*(WORDS/2)-1 downto WORD_BITS*0)
            );

        SECOND: merge generic map (WORDS/2, WORD_BITS, COMP_HIGH, COMP_LOW, INFO_BITS)
            port map (
                CLK     => CLK,
                RST     => RST,
                CLR     => CLR,
                I_SORT  => q_sort,
                I_UP    => q_up,
                I_INFO  => q_info,
                I_DATA  => q_data(WORD_BITS*(WORDS)-1 downto WORD_BITS*(WORDS/2)),
                O_SORT  => open,
                O_UP    => open,
                O_INFO  => open,
                O_DATA  => O_DATA(WORD_BITS*(WORDS)-1 downto WORD_BITS*(WORDS/2))
            );
    end generate;
end rtl;