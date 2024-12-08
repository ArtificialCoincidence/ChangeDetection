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

begin

	process(clk, thrs_rst)
		variable pixel_count : integer := 0;
   begin
		if thrs_rst = '1' then
			address_ramp <= (others => '0');
		elsif rising_edge(clk) then
			if thrs_en = '1' then
				address_ramp <= std_logic_vector(to_unsigned(pixel_count, address_ramp'length));
					if pixel_count >= tot_pixels then
						pixel_count := 0;
					end if;
				pixel_count := pixel_count + 1;
			end if;
		end if;
	end process;
	
	rd_addr <= address_ramp;
	bram_addr <= address_ramp;
	
	process(rd_addr, thrs_rst)
		variable cumulative : integer := 0;
		constant quantile3  : integer := tot_pixels*3/4;
		constant quantile4  : integer := tot_pixels;
	begin
		if thrs_rst = '1' then
			cumulative := 0;
			thrs_sig <= '0';
			thrs_sig <= '0';
		elsif thrs_en = '1' then
			cumulative := cumulative + to_integer(unsigned(data_in));
			if cumulative >= quantile3 and cumulative <= quantile4 then
				thrs_sig <= '1';
			else
				thrs_sig <= '0';
			end if;
		end if;
		cumulative_sum <= std_logic_vector(to_unsigned(cumulative, cumulative_sum'length));
	end process;
	
	process(thrs_sig, thrs_rst)
	begin
		if thrs_rst = '1' then
			thrs_val <= (others => '0');
		elsif rising_edge(thrs_sig) then
			thrs_val <= rd_addr;
      end if;
	end process;

end rtl;