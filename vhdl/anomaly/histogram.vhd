library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity histogram is
    generic(
        IM_SIZE     : integer;
        ADDR_SIZE   : integer;
        WORD_SIZE   : integer
    );
    port (
        clk         : in std_logic;
        rstram      : in std_logic; -- rst
        start_cntr  : in std_logic; -- data valid in
        wren        : out std_logic;  -- write RAM enable, use to pause stream

        addrin      : in std_logic_vector(ADDR_SIZE-1 downto 0) ; -- device data as address for RAM
        data_in      : in std_logic_vector (WORD_SIZE-1 downto 0); -- RAM data out
        data_out    : out std_logic_vector(WORD_SIZE-1 downto 0); -- RAM data in
        ramwraddr   : out std_logic_vector(ADDR_SIZE-1 downto 0) -- BRAM write address: delayed addrin or ramp
	);
end histogram;

architecture hlsm of histogram is

    constant cntr_value : std_logic_vector(6 downto 0) := "1100100";   -- make it generic

    signal wr_addr                      : std_logic_vector(ADDR_SIZE-1 downto 0);
    signal pre_cntr, next_cntr          : std_logic_vector(ADDR_SIZE-1 downto 0); -- num of samples for which histogram is computed.
    signal pre_addrcnt, next_addrcnt    : std_logic_vector(ADDR_SIZE-1 downto 0);
    signal addr                         : std_logic_vector(ADDR_SIZE-1 downto 0);

begin
    addr <= pre_addrcnt when rstram = '1' else addrin; 

    process(clk, rstram)
    begin
        if rising_edge(clk) then
            if(rstram = '1' or start_cntr = '0') then
                pre_cntr <= (others => '0');
            else 
                pre_cntr <= next_cntr;
            end if;

            if(start_cntr = '0') then
                pre_addrcnt <= (others => '0');
            else				
                pre_addrcnt <= next_addrcnt;
            end if;

            wr_addr <= addr; -- delay write address
        end if;
    end process;

    process(data_in, rstram, pre_cntr)
    begin
        if(pre_cntr >= cntr_value) then
            next_cntr <= pre_cntr;
            wren <= '0';
        else
            wren <= '1';
            next_cntr <= pre_cntr + '1';
        end if;

        if(rstram = '1' or start_cntr = '0') then
            data_out <= (others => '0');
        else
            if(data_in = "1110") then -- prevent overflow
                data_out <= data_in;
            else
                data_out <= data_in + '1';
            end if;
        end if;

        ramwraddr <= wr_addr;
    end process;

    process(start_cntr, pre_addrcnt) -- generate ramp to clear BRAM
    begin
        next_addrcnt <= pre_addrcnt + '1';
    end process;
end hlsm;