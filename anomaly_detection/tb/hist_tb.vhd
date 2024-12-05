library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity hist_tb is
end hist_tb;

architecture test of hist_tb is

   component anomaly_fsm is
	generic(
		ADDR_SIZE   : integer := 16;
		WORD_SIZE   : integer := 16
	);
	port (
		clk             : in  std_logic;
		hist_rst        : in  std_logic;
		hist_en         : in  std_logic;
		thres_en        : in  std_logic;
		ram_addr        : in  std_logic_vector(ADDR_SIZE-1 downto 0); -- read address from threshold or address for reset
		data_in         : in  std_logic_vector(ADDR_SIZE-1 downto 0); -- device data as read address for RAM
		data_out        : out std_logic_vector(ADDR_SIZE-1 downto 0)  -- data to threshold
	);
   end component;

   signal clk_tb       : std_logic := '1';
   signal rst_tb       : std_logic := '0';
	signal en_hist_tb   : std_logic;
   signal en_thres_tb  : std_logic;
	signal ram_add_tb   : std_logic_vector(15 downto 0);
	signal data_in_tb   : std_logic_vector(15 downto 0);
   signal data_out_tb  : std_logic_vector(15 downto 0);
   
begin
  
	dut: anomaly_fsm
	generic map(
		ADDR_SIZE     => 16,
		WORD_SIZE     => 16
	)
	port map(
		clk             => clk_tb,
		hist_rst        => rst_tb,
		hist_en         => en_hist_tb,
		thres_en        => en_thres_tb,
		ram_addr        => ram_add_tb,
		data_in         => data_in_tb,
		data_out			 => data_out_tb
    );

    clk_tb <= not clk_tb after 0.5 ns;

	-- PROCESS FOR TESTING
	stim: process
	begin
		rst_tb <= '1', '0' after 10 ns;
		en_hist_tb <= '0', '1' after 10 ns;
		en_thres_tb <= '0';
		ram_add_tb <= (others => '0');
		
		wait for 10 ns;
		
		for i in 0 to 10 loop
			data_in_tb <= std_logic_vector(to_unsigned(i, data_in_tb'length));
			wait for 1 ns;
		end loop;
      
      for j in 0 to 10 loop
			data_in_tb <= std_logic_vector(to_unsigned(3, data_in_tb'length));
			wait for 1 ns;
		end loop;
		
		for i in 0 to 10 loop
			data_in_tb <= std_logic_vector(to_unsigned(i, data_in_tb'length));
			wait for 1 ns;
		end loop;
        
      wait for 5 ns;
		
      en_hist_tb <= '0';
		en_thres_tb <= '1';
		
		for x in 0 to 10 loop
			ram_add_tb <= std_logic_vector(to_unsigned(x, ram_add_tb'length));
			wait for 1 ns;
		end loop;
		
		en_thres_tb <= '0';
		rst_tb <= '1';
		
		for z in 0 to 10 loop
			ram_add_tb <= std_logic_vector(to_unsigned(z, ram_add_tb'length));
			wait for 1 ns;
		end loop;
      
      wait;

    end process stim;
end test;
