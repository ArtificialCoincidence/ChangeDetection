library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.fixed_pkg.all;

entity tb_exchange is
end tb_exchange;

architecture test of tb_exchange is

  component exchange is
    generic (
        WORD_BITS :  integer := 32;
        COMP_BITS :  integer := 32
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

  constant WORD_BITS : integer := 32;
  constant COMP_BITS : integer := 32;
  signal I_SORT   :  std_logic;
  signal I_UP     :  std_logic;
  signal I_A      :  ufixed(-1 downto -WORD_BITS);
  signal I_B      :  ufixed(-1 downto -WORD_BITS);
  signal O_A      :  ufixed(-1 downto -WORD_BITS);
  signal O_B      :  ufixed(-1 downto -WORD_BITS);
  
begin
  
  dut: exchange
    generic map (
        WORD_BITS => WORD_BITS,
        COMP_BITS => COMP_BITS
    )
    port map (
        I_SORT    => I_SORT,
        I_UP      => I_UP,
        I_A       => I_A,
        I_B       => I_B,
        O_A       => O_A,
        O_B       => O_B
    );
  
  -- PROCESS FOR TESTING
  stim: process
  begin
    I_A <= "01100100001010101101001000111100";  -- 0.3912784
    I_B <= "11001000101010100100000110001000";  -- 0.7838479
    I_SORT <= '1';
    I_UP <= '1';
    wait for 10 ns;
    I_UP <= '0';
    wait;
  end process stim;

end test;
