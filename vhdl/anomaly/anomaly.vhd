library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity anomaly is
    generic(
        IM_SIZE 	: integer;
		ADDR_SIZE 	: integer;
		WORD_SIZE 	: integer
    );
    port (
		clk 		: in  std_logic;
		rst			: in  std_logic;
		data_valid	: in  std_logic;
		img_data_i	: in  std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data in
		img_data_o	: out std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data out
		e_o_i		: out std_logic
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
			clk 		: in  std_logic;
			rstram 		: in  std_logic; 	-- rst
			start_cntr 	: in  std_logic; 	-- data valid in
			wren 		: out std_logic;  	-- write RAM enable, use to pause stream
			addrin 		: in  std_logic_vector(ADDR_SIZE-1 downto 0) ; 	-- device data as address for RAM
			data_in 	: in  std_logic_vector (WORD_SIZE-1 downto 0); 	-- RAM data out
			data_out 	: out std_logic_vector(WORD_SIZE-1 downto 0); 	-- RAM data in
			ramwraddr 	: out std_logic_vector(ADDR_SIZE-1 downto 0) 	-- BRAM write address: delayed addrin or ramp
		);
	end component;
	
	component bram_histogram is
		generic(
			ADDR_SIZE  	: integer;
			WORD_SIZE  	: integer
		);
		port (
			clk 		: in  std_logic;
			data_valid	: in  std_logic;
			data		: in  std_logic_vector(WORD_SIZE-1 downto 0);
			rdaddress	: in  std_logic_vector(ADDR_SIZE-1 downto 0);
			wraddress	: in  std_logic_vector(ADDR_SIZE-1 downto 0);
			wren		: in  std_logic;
			q			: out std_logic_vector(WORD_SIZE-1 downto 0)
		);
	end component;

	component threshold_cdf is
		generic (
			IM_SIZE     : integer;
			WORD_SIZE   : integer;    -- Width of each histogram bin (number of bits)
			ADDR_SIZE   : integer     -- Log of number of histogram bins
		);
		port (
			clk         : in  std_logic;
			rst         : in  std_logic;
			enable      : in  std_logic;    -- histogram complete, start executing, use negated data valid
			start_scan  : out std_logic;    -- values calulated, start scanning image
			thrs1       : out std_logic_vector(ADDR_SIZE-1 downto 0);
			datain      : in  std_logic_vector(WORD_SIZE-1 downto 0);  -- Histogram value from BRAM
			rdaddr      : out std_logic_vector(ADDR_SIZE-1 downto 0)  -- Output threshold value (address)
		);
	end component;

	component scan_image is
		generic(
			IM_SIZE     : integer;
			ADDR_SIZE   : integer;
			WORD_SIZE   : integer
		);
		port(
			clk 	    : in  std_logic;
			rst         : in  std_logic;
			enable		: in  std_logic;     -- data valid signal negated
			start       : in  std_logic;
			thrs1       : in  std_logic_vector(ADDR_SIZE-1 downto 0);
			data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);    -- from memory (original image)
			data_out	: out std_logic_vector(WORD_SIZE-1 downto 0)    -- back to memory
			e_o_i		: out std_logic;
		);
	end component;
	
	signal ram_data_o, ram_data_i 					: std_logic_vector (WORD_SIZE-1 downto 0);
	signal wraddress, rdaddress, rdaddr_mux, thrs1	: std_logic_vector (ADDR_SIZE-1 downto 0);
	signal wren, start								: std_logic;
	
begin
	
	HIST: histogram
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, wren, img_data_i, ram_data_o, ram_data_i, wraddress);
	
	BRAM: bram_histogram
	generic map(ADDR_SIZE, WORD_SIZE)
	port map(clk, data_valid, ram_data_i, rdaddr_mux, wraddress, wren, ram_data_o);

	THRS: threshold_cdf
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, start, thrs1, ram_data_o, rdaddress);

	SCAN: scan_image
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, start, thrs1, img_data_i, img_data_o, e_o_i);

	rdaddr_mux <= img_data_i when data_valid = '1' else rdaddress;
	
end rtl;