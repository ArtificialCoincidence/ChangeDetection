library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

-- TO DO: 
-- add pulse gen?
-- calculate quantile index
-- generate data out
-- redo structure


entity anomaly is
    generic(
        IM_SIZE : integer := 250 
    );
    port (
			clk 		: in std_logic;
			rst			: in std_logic;	
			data_valid	: in std_logic;
			img_data_i	: in std_logic_vector(15 downto 0); -- pixel data in
			img_data_o	: out std_logic_vector(15 downto 0) -- pixel data out
	);
end anomaly;

architecture rtl of anomaly is

	component histogram is
		generic(
			IM_SIZE : integer := 250
		);
		port (
			clk : in std_logic;
			rstram : in std_logic; -- rst
			start_cntr : in std_logic; -- data valid in
			wren : out std_logic;  -- write RAM enable, use to pause stream
	
			addrin : in std_logic_vector(14 downto 0) ; -- device data as address for RAM
			datain : in std_logic_vector (15 downto 0); -- RAM data out
			data_out : out std_logic_vector(15 downto 0); -- RAM data in
			ramwraddr : out std_logic_vector(14 downto 0) -- BRAM write address: delayed addrin or ramp
		);
	end component;
	
	component BRAM_histogram is
		port (
			clock		: IN STD_LOGIC;
			data		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			rdaddress	: IN STD_LOGIC_VECTOR (14 DOWNTO 0);
			wraddress	: IN STD_LOGIC_VECTOR (14 DOWNTO 0);
			wren		: IN STD_LOGIC;
			q			: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
	
	signal ram_data_o, ram_data_i : std_logic_vector (15 downto 0);
	signal wraddress, input_data_res : std_logic_vector (14 downto 0);
	signal wren, rstram : std_logic; 
	
begin

	input_data_res <= img_data_i(15 downto 1);
	
	HIST: histogram
	generic map(IM_SIZE)
	port map(clk, rstram, data_valid, wren, input_data_res, ram_data_o, ram_data_i, wraddress);
	
	BRAM: BRAM_histogram
	port map(clk, ram_data_i, input_data_res, wraddress, wren, ram_data_o);
	
end rtl;
	