library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity threshold_cdf is
	generic (
		IM_SIZE		: integer := 500;
		WORD_SIZE   : integer := 16;    -- Width of each histogram bin
		ADDR_SIZE   : integer := 16     -- Log of number of histogram bins
	);
	port (
		clk         : in  std_logic;
		thrs_rst    : in  std_logic;
		thrs_en     : in  std_logic;    -- histogram complete, start executing, use negated data valid
		thrs_sig    : out std_logic;    -- values calulated, start scanning image
		thrs_val    : out std_logic_vector(ADDR_SIZE-1 downto 0);
		data_in     : in  std_logic_vector(WORD_SIZE-1 downto 0);  	-- Histogram value from BRAM
		bram_addr	: out std_logic_vector(ADDR_SIZE-1 downto 0)
	);
end threshold_cdf;

architecture rtl of threshold_cdf is

    constant tot_pixels     : integer := IM_SIZE;
	 
	 signal rd_addr			 : std_logic_vector(ADDR_SIZE-1 downto 0);
	 signal address_ramp     : std_logic_vector(ADDR_SIZE-1 downto 0);
    signal cumulative_sum   : std_logic_vector(WORD_SIZE-1 downto 0);
	 signal thrs_en_del		 : std_logic;
	 signal outlayer			 : std_logic;

begin

	process(clk, thrs_rst)
		variable pixel_count : integer := 1;
   begin
		if thrs_rst = '1' then
			address_ramp <= (others => '0');
		elsif rising_edge(clk) then
			if thrs_en = '1' then
				pixel_count := pixel_count + 1;
				if pixel_count >= tot_pixels then
					pixel_count := 1;
				end if;
			end if;
			address_ramp <= std_logic_vector(to_unsigned(pixel_count, address_ramp'length));
			thrs_en_del <= thrs_en;
		end if;
	end process;
	
	rd_addr <= address_ramp;
	bram_addr <= address_ramp;
	
	process(rd_addr, thrs_rst)
		constant quantile3  : integer := tot_pixels*3/4;
		constant quantile4  : integer := tot_pixels;
	begin
		if thrs_rst = '1' then
			cumulative_sum <= (others => '0');
			thrs_sig <= '0';
			outlayer <= '0';
		elsif thrs_en_del = '1' then
			cumulative_sum <= cumulative_sum + data_in;
			if cumulative_sum >= quantile3 and cumulative_sum <= quantile4 then
				thrs_sig <= '1';
				outlayer <= '1';
			else
				thrs_sig <= '0';
				outlayer <= '0';
			end if;
		end if;
	end process;
	
	process(outlayer, thrs_rst)
	begin
		if thrs_rst = '1' then
			thrs_val <= (others => '0');
		elsif rising_edge(outlayer) then
			thrs_val <= rd_addr;
      end if;
	end process;

end rtl;