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
        IM_SIZE : integer := 250	-- 
    );
    port (
			clk 			: in std_logic;
			rst			: in std_logic;
			
			rstram 		: in std_logic;
			start_cntr	: in std_logic;  
			sel_data 	: in std_logic;
			rstcntr 		: in std_logic;			

			img_data_i	: in std_logic_vector(14 downto 0); -- pixel data in
			img_data_o	: out std_logic_vector(14 downto 0) -- pixel data out
	);
end anomaly;

architecture rtl of anomaly is

	component histogram is
		 generic(
			  IM_SIZE : integer := 250
		 );
		 port (
			clk 			: in std_logic;

			addrin 		: in std_logic_vector(14 downto 0) ; -- pixel data as address for RAM
			datain 		: in std_logic_vector (15 downto 0); -- RAM data out
			data_out 	: out std_logic_vector(15 downto 0); -- RAM data in
			ramwraddr 	: out std_logic_vector(14 downto 0); -- BRAM write address: delayed addrin or ramp
		  
			rstcntr 		: in std_logic;
			rstram 		: in std_logic;
			start_cntr	: in std_logic;  
			sel_data 	: in std_logic;  -- sel BRAM write address (normal or reset)
			wren 			: out std_logic  -- write RAM enable
		);
	end component;
	
	component BRAM_histogram is
		port
		(
			clock			: IN STD_LOGIC;
			data			: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			rdaddress	: IN STD_LOGIC_VECTOR (14 DOWNTO 0);
			wraddress	: IN STD_LOGIC_VECTOR (14 DOWNTO 0);
			wren			: IN STD_LOGIC;
			q				: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
	
	signal ram_data_o, ram_data_i : std_logic_vector (15 downto 0);
	signal wraddress : std_logic_vector (14 downto 0);
	signal wren : std_logic;
	
begin
	
	HIST: histogram
	generic map(IM_SIZE)
	port map(clk, img_data_i, ram_data_o, ram_data_i, wraddress, rstcntr, rstram, start_cntr, sel_data, wren);
	
	BRAM: BRAM_histogram
	port map(clk, ram_data_i, img_data_i, wraddress, wren, ram_data_o);
	
end rtl;
	