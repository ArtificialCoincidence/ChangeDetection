library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity anomaly is
    generic(
		IM_SIZE 	: integer := 500;
		ADDR_SIZE 	: integer := 16;
		WORD_SIZE 	: integer := 16
    );
    port (
		clk 		: in  std_logic;
		rst			: in  std_logic;
		data_valid	: in  std_logic;
		img_data_i	: in  std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data in
		img_data_o	: out std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data out
		addr_out    : out std_logic_vector(ADDR_SIZE-1 downto 0);
		e_o_i			: out std_logic
	);
end anomaly;

architecture rtl of anomaly is

	component histogram is
		generic(
			IM_SIZE   	: integer;
			ADDR_SIZE 	: integer;
			WORD_SIZE 	: integer
		);
		port (
			clk 			: in  std_logic;
			rstram 		: in  std_logic;
			start_cntr 	: in  std_logic;
			wren 			: out std_logic;
			addrin 		: in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in 		: in  std_logic_vector(WORD_SIZE-1 downto 0);
			data_out 	: out std_logic_vector(WORD_SIZE-1 downto 0);
			ramwraddr 	: out std_logic_vector(ADDR_SIZE-1 downto 0)
		);
	end component;
	
	component bram_histogram is
	generic(
		ADDR_SIZE   : integer;
		WORD_SIZE   : integer
	);
	port
	(
		clk			: in std_logic;
		data			: in std_logic_vector (15 downto 0);
		rdaddress	: in std_logic_vector (15 downto 0);
		wraddress	: in std_logic_vector (15 downto 0);
		wren			: in std_logic;
		q				: out std_logic_vector (15 downto 0)
	);
	end component;

	component threshold_cdf is
		generic (
			IM_SIZE     : integer;
			WORD_SIZE   : integer;
			ADDR_SIZE   : integer
		);
		port (
			clk         : in  std_logic;
			rst         : in  std_logic;
			enable      : in  std_logic;
			start_scan  : out std_logic;
			thrs1       : out std_logic_vector(ADDR_SIZE-1 downto 0);
			datain      : in  std_logic_vector(WORD_SIZE-1 downto 0);
			rdaddr      : out std_logic_vector(ADDR_SIZE-1 downto 0)
		);
	end component;

	component scan_image is
		generic(
			IM_SIZE     : integer;
			ADDR_SIZE   : integer;
			WORD_SIZE   : integer
		);
		port(
			clk 	 		: in  std_logic;
			rst      	: in  std_logic;
			enable		: in  std_logic;
			start    	: in  std_logic;
			thrs1    	: in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in  	: in  std_logic_vector(WORD_SIZE-1 downto 0);
			data_out		: out std_logic_vector(WORD_SIZE-1 downto 0);
			addr_out    : out std_logic_vector(ADDR_SIZE-1 downto 0);
			e_o_i			: out std_logic
		);
	end component;
	
	signal ram_data_o, ram_data_i 						: std_logic_vector (WORD_SIZE-1 downto 0);
	signal wraddress, rdaddress, rdaddr_mux, thrs1	: std_logic_vector (ADDR_SIZE-1 downto 0);
	signal wren, start										: std_logic;
	
begin
	
	HIST: histogram
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, wren, img_data_i, ram_data_o, ram_data_i, wraddress);
	
	BRAM: bram_histogram
	generic map(ADDR_SIZE, WORD_SIZE)
	port map(clk, ram_data_i, rdaddr_mux, wraddress, wren, ram_data_o);

	THRS: threshold_cdf
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, start, thrs1, ram_data_o, rdaddress);

	SCAN: scan_image
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, start, thrs1, img_data_i, img_data_o, addr_out, e_o_i);

	rdaddr_mux <= img_data_i when data_valid = '1' else rdaddress;
	
end rtl;