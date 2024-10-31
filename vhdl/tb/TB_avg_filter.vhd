library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.fixed_pkg.all;

entity TB_avg_filter is
end TB_avg_filter;

architecture test of TB_avg_filter is

  component avg_filter is
    generic(
        V_SIZE: integer := 10;    -- vertical size image
        H_SIZE: integer := 10;    -- horizontal size image
        K_SIZE: integer := 3);   -- size of the kernel
    
    port(
        clk         : in std_logic;
        rst         : in std_logic;

        data_in     : in std_logic_vector(31 downto 0); --ufixed(-1 downto -32);
        i_data_en   : in std_logic;

        data_out    : out ufixed(-1 downto -32);
        o_valid     : out std_logic
        );   
  end component;

  component read_data is
    generic(
          file_path     	  : string := "";
          ENTRIES		        : integer := 100;
          WORD_SIZE	        : integer := 32;
          ADDR_SIZE	        : integer := 32
          );

    port(
          rst         : in std_logic;
          addr		    : in std_logic_vector(ADDR_SIZE - 1 downto 0);
          enable			: in std_logic;
          data_out		: out std_logic_vector(ADDR_SIZE - 1 downto 0)
          );
  end component;

  signal clk_tb : std_logic := '0';
  signal rst_tb : std_logic := '0';
  signal data_in_tb   : std_logic_vector(31 downto 0);--ufixed(-1 downto -32);
  signal enable : std_logic;
  signal data_out_tb : ufixed(-1 downto -32);
  signal address : std_logic_vector(31 downto 0);
  signal o_valid_tb : std_logic;
  
begin
  
  dut: avg_filter
    generic map (
        V_SIZE => 10,    -- vertical size image
        H_SIZE => 10,    -- horizontal size image
        K_SIZE => 3
    )
    port map (
        clk           => clk_tb,
        rst           => rst_tb,
        data_in       => data_in_tb,
        i_data_en     => enable,
        data_out      => data_out_tb,
        o_valid       => o_valid_tb
    );

  dmem: read_data
    generic map(
        file_path       => "tb/files/datafile1.mem",
        ENTRIES         => 100,
        WORD_SIZE       => 32,
        ADDR_SIZE       => 32
    )
    port map(
        rst             => rst_tb,
        addr            => address,
        enable          => enable,
        data_out        => data_in_tb
    );


  clk_tb <= not clk_tb after 1 ns;
  rst_tb <= '0', '1' after 5.5 ns;
  
  -- PROCESS FOR TESTING
  stim: process
  begin
    rst_tb <= '0', '1' after 5.5 ns; 
    enable <= '1' after 10 ns;
    address <= "00000000000000000000000000000000";
    wait for 5.5 ns;

    for i in 0 to 99 loop
      address <= std_logic_vector(unsigned(address) + 1);
      wait for 5.5 ns;
    end loop;
  end process stim;

end test;
