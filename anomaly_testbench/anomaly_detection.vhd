library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity anomaly_detection is
	generic(
		IM_SIZE 		: integer := 500;
		ADDR_SIZE 	: integer := 16;
		WORD_SIZE 	: integer := 16;
		COUNT_WIDTH	: integer := 9
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
			thrs_en    	: in  std_logic;
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
			data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);
			bram_addr	: out std_logic_vector(ADDR_SIZE-1 downto 0)
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
	
	component control_fsm is
		generic(
			COUNT_WIDTH : integer := 9
		);
		port(
			clk 					: in  std_logic;
			rst					: in  std_logic;
			ready_out			: out std_logic;
			startpacket_in		: in  std_logic;
			endpacket_in		: in  std_logic;
			valid_in				: in  std_logic;
			ready_in				: in  std_logic;
			startpacket_out	: out std_logic;
			endpacket_out		: out std_logic;
			valid_out			: out std_logic;
			hist_en				: out std_logic;
			bram_rd_en			: out std_logic;
			bram_wr_en			: out std_logic;
			thrs_en				: out std_logic;
			scan_en				: out std_logic;
			hist_rst				: out std_logic;
			thrs_sig				: in  std_logic;
			addr_gen				: out std_logic_vector(COUNT_WIDTH-1 downto 0)
		);
	end component;

	-- ENABLES
	signal hist_en_sig		: std_logic;
	signal bram_rd_en_sig	: std_logic;
	signal bram_wr_en_sig	: std_logic;
	signal thrs_en_sig		: std_logic;
	signal scan_en_sig		: std_logic;
	-- RESET HST
	signal hist_rst_sig		: std_logic;
	-- THR SIG and VAL
	signal thrs_sig_sig		: std_logic;
	signal thrs_val_sig		: std_logic_vector(ADDR_SIZE-1 downto 0);
	-- ADDR
	signal addr_gen_sig		: std_logic_vector(COUNT_WIDTH-1 downto 0);
	signal bram_addr_sig		: std_logic_vector(ADDR_SIZE-1 downto 0);
	-- DATA
	signal data_hist_sig		: std_logic_vector(WORD_SIZE-1 downto 0);
	signal data_bram_sig		: std_logic_vector(WORD_SIZE-1 downto 0);
	
begin
	
	HIST: histogram
	generic map(
		ADDR_SIZE 	=> ADDR_SIZE,
		WORD_SIZE 	=> WORD_SIZE
	)
	port map(
		clk         => clk,
		hist_rst    => hist_rst_sig,
		hist_en     => hist_en_sig,
		thrs_en    	=> thrs_en_sig,
		ram_addr    => bram_addr_sig,
		data_in     => data_in,
		data_out		=> data_hist_sig
	);
	
	BRAM: bram_row
	port map(
		address		=> addr_gen_sig,
		clock			=> clk,
		data			=> data_in,
		rden			=> bram_rd_en_sig,
		wren			=> bram_wr_en_sig,
		q				=> data_bram_sig
	);

	THRS: threshold_cdf
	generic map(
		IM_SIZE		=> IM_SIZE,
		ADDR_SIZE	=> ADDR_SIZE,
		WORD_SIZE	=> WORD_SIZE
	)
	port map(
		clk         => clk,
		thrs_rst    => rst,
		thrs_en     => thrs_en_sig,
		thrs_sig    => thrs_sig_sig,
		thrs_val    => thrs_val_sig,
		data_in     => data_hist_sig,
		bram_addr	=> bram_addr_sig
	);

	SCAN: scan_image
	generic map(
		ADDR_SIZE	=> ADDR_SIZE,
		WORD_SIZE	=> WORD_SIZE
	)
	port map(
		clk 	    	=> clk,
		scan_rst   	=> rst,
		scan_en		=> scan_en_sig,
		thrs_val    => thrs_val_sig,
		data_in     => data_bram_sig,
		data_out		=> data_out
	);

	FSM: control_fsm
	generic map(
		COUNT_WIDTH	=> COUNT_WIDTH
	)
	port map(
		clk 					=> clk,
		rst					=> rst,
		ready_out			=> ready_out,
		startpacket_in		=> startpacket_in,
		endpacket_in		=> endpacket_in,
		valid_in				=> valid_in,
		ready_in				=> ready_in,
		startpacket_out	=> startpacket_out,
		endpacket_out		=> endpacket_out,
		valid_out			=> valid_out,
		hist_en				=> hist_en_sig,
		bram_rd_en			=> bram_rd_en_sig,
		bram_wr_en			=> bram_wr_en_sig,
		thrs_en				=> thrs_en_sig,
		scan_en				=> scan_en_sig,
		hist_rst				=> hist_rst_sig,
		thrs_sig				=> thrs_sig_sig,
		addr_gen				=> addr_gen_sig
	);
	
end rtl;