library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

-- TO DO:
-- calculate quantile index
-- generate data out

entity anomaly is
    generic(
        IM_SIZE : integer := 250;
		ADDR_SIZE : integer := 32;
		WORD_SIZE : integer := 32
    );
    port (
		clk 		: in std_logic;
		rst			: in std_logic;
		data_valid	: in std_logic;
		img_data_i	: in std_logic_vector(WORD_SIZE-1 downto 0); -- pixel data in
		img_data_o	: out std_logic_vector(WORD_SIZE-1 downto 0) -- pixel data out
	);
end anomaly;

architecture rtl of anomaly is

	component histogram is
		generic(
			IM_SIZE : integer := 250;
			ADDR_SIZE : integer := 32;
			WORD_SIZE : integer := 32
		);
		port (
			clk : in std_logic;
			rstram : in std_logic; -- rst
			start_cntr : in std_logic; -- data valid in
			wren : out std_logic;  -- write RAM enable, use to pause stream
	
			addrin : in std_logic_vector(ADDR_SIZE-1 downto 0) ; -- device data as address for RAM
			datain : in std_logic_vector (WORD_SIZE-1 downto 0); -- RAM data out
			data_out : out std_logic_vector(WORD_SIZE-1 downto 0); -- RAM data in
			ramwraddr : out std_logic_vector(ADDR_SIZE-1 downto 0) -- BRAM write address: delayed addrin or ramp
		);
	end component;
	
	component bram_histogram is
		generic(
			ADDR_SIZE : integer := 32;
			WORD_SIZE : integer := 32
		);
		port (
			clk 			: in std_logic;
			data_valid		: in std_logic;
			data			: in std_logic_vector(WORD_SIZE-1 downto 0);
			rdaddress		: in std_logic_vector(ADDR_SIZE-1 downto 0);
			wraddress		: in std_logic_vector(ADDR_SIZE-1 downto 0);
			wren			: in std_logic;
			q				: out std_logic_vector(WORD_SIZE-1 downto 0)
		);
	end component;
	
	signal ram_data_o, ram_data_i : std_logic_vector (WORD_SIZE-1 downto 0);
	signal wraddress : std_logic_vector (ADDR_SIZE-1 downto 0);
	signal wren : std_logic; 
	
begin
	
	HIST: histogram
	generic map(IM_SIZE, ADDR_SIZE, WORD_SIZE)
	port map(clk, rst, data_valid, wren, img_data_i, ram_data_o, ram_data_i, wraddress);
	
	BRAM: bram_histogram
	generic map(ADDR_SIZE, WORD_SIZE)
	port map(clk, data_valid, ram_data_i, img_data_i, wraddress, wren, ram_data_o);
	
end rtl;
	