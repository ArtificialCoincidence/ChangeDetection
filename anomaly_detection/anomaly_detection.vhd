library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity anomaly_detection is
	generic(
		IM_SIZE 		: integer := 500;
		ADDR_SIZE 	: integer := 16;
		WORD_SIZE 	: integer := 16
	);
	port (
		clk 					: in  std_logic;
		rst					: in  std_logic;
		-- SINK
		ready_out			: out std_logic;
		data_in				: in  std_logic_vector(WORD_SIZE-1 downto 0);
		startpacket_in		: in  std_logic;
		endpacket_in		: in  std_logic;
		valid_in				: in  std_logic;
		-- SOURCE
		ready_in				: in  std_logic;
		data_out				: out std_logic_vector(WORD_SIZE-1 downto 0);
		startpacket_out	: out std_logic;
		endpacket_out		: out std_logic;
		valid_out			: out std_logic
	);
end anomaly_detection;

architecture rtl of anomaly_detection is
	
	component histogram is
		generic(
			ADDR_SIZE   : integer := 16;
			WORD_SIZE   : integer := 16
		);	
		port (
			clk         : in  std_logic;
			hist_rst    : in  std_logic;
			hist_en     : in  std_logic;
			thres_en    : in  std_logic;
			ram_addr    : in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in     : in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_out    : out std_logic_vector(ADDR_SIZE-1 downto 0)
		);
	end component;

	component bram_row is
		port(
			address		: in  std_logic_vector(8 downto 0);
			clock			: in  std_logic;
			data			: in  std_logic_vector(15 downto 0);
			rden			: in  std_logic;
			wren			: in  std_logic;
			q				: out std_logic_vector(15 downto 0)
		);
	end component;
	
	component threshold_cdf is
		generic (
			IM_SIZE		: integer := 500;
			WORD_SIZE   : integer := 16;
			ADDR_SIZE   : integer := 16
		);
		port (
			clk         : in  std_logic;
			thrs_rst    : in  std_logic;
			thrs_en     : in  std_logic;
			thrs_sig    : out std_logic;
			thrs_val    : out std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0)
		);
	end component;
	
	component scan_image is
		generic(
			ADDR_SIZE   : integer := 16;
			WORD_SIZE   : integer := 16
		);
		port(
			clk 	    	: in  std_logic;
			scan_rst   	: in  std_logic;
			scan_en		: in  std_logic;
			thrs_val    : in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);
			data_out		: out std_logic_vector(WORD_SIZE-1 downto 0)
		);
	end component;


	signal 
	
begin
	
	HIST: histogram
	generic map(ADDR_SIZE, WORD_SIZE)
	port map();
	
	BRAM: bram_row
	port map();

	THRS: threshold_cdf
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map();

	SCAN: scan_image
	generic map(ADDR_SIZE, WORD_SIZE)
	port map();
	
end rtl;