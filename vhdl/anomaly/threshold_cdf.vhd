library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity threshold_cdf is
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
end threshold_cdf;

architecture behavioral of threshold_cdf is

    constant tot_pixels     : integer := IM_SIZE;

    signal address_ramp     : unsigned(ADDR_SIZE-1 downto 0) := (others => '0');
    signal cumulative_sum   : unsigned(WORD_SIZE-1 downto 0) := (others => '0');
    signal outlayer         : std_logic;

begin

    process(clk, rst)
        variable pixel_count : integer := 0;
    begin
        if rst = '1' then
            address_ramp <= (others => '0');
        elsif rising_edge(clk) then
            if enable = '0' then
                address_ramp <= to_unsigned(pixel_count, address_ramp'length);
                if pixel_count >= tot_pixels then
                    pixel_count := 0;
                end if;
                pixel_count := pixel_count + 1;
            end if;
        end if;
    end process;

    rdaddr <= std_logic_vector(address_ramp);

    process(datain, rst)
        variable cumulative : integer := 0;
        constant quantile3  : integer := tot_pixels*3/4;
        constant quantile4  : integer := tot_pixels;
    begin
        if rst = '1' then
            cumulative := 0;
            outlayer <= '0';
        elsif enable = '0' then
            cumulative := cumulative + to_integer(unsigned(datain));
            if cumulative >= quantile3 and cumulative <= quantile4 then
                outlayer <= '1';
            else
                outlayer <= '0';
            end if;
        end if;

        cumulative_sum <= to_unsigned(cumulative, cumulative_sum'length);
    end process;

    process(outlayer, rst)
    begin
        if rst = '1' then
            thrs1 <= (others => '0');
            start_scan <= '0';
        else
            if rising_edge(outlayer) then
                thrs1 <= std_logic_vector(address_ramp - 1);
                start_scan <= '1';
            end if;
        end if;
    end process;

end behavioral;