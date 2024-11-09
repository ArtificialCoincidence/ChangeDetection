library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity histogram is
    generic(
        IM_SIZE : integer := 250
    );
    port (
        clk : in std_logic;
        rstram : in std_logic; -- rst
        start_cntr : in std_logic; -- data valid in
        wren : out std_logic;  -- write RAM enable, use to pause stream

        addrin : in std_logic_vector(14 downto 0) ; -- device data as address for RAM
        datain : in std_logic_vector (14 downto 0); -- RAM data out
        data_out : out std_logic_vector(14 downto 0); -- RAM data in
        ramwraddr : out std_logic_vector(14 downto 0) -- BRAM write address: delayed addrin or ramp
	);
end histogram;

architecture hlsm of histogram is

    constant cntr_value : integer := IM_SIZE*IM_SIZE;

    signal wr_addr, wr_addr1 : std_logic_vector(14 downto 0);
    signal pre_cntr, next_cntr : std_logic_vector(14 downto 0); -- count num of samples for which histogram to be computed.
    signal pre_addrcnt, next_addrcnt: std_logic_vector(14 downto 0);
    signal addr : std_logic_vector(14 downto 0);

begin
    addr <= pre_addrcnt when rstram = '1' else addrin; 

    process(clk,rstram)
    begin
        if rising_edge(clk) then
            if(rstram = '1') then
                pre_cntr <= (others => '0');
            else 
                pre_cntr <= next_cntr;
            end if;

            if(start_cntr = '0') then
                pre_addrcnt <= (others => '0');
            else				
                pre_addrcnt <= next_addrcnt;
            end if;

            wr_addr1 <= addr;	
            wr_addr <= wr_addr1; -- delay write address by 2 clock
        end if;
    end process;

    process(datain, rstram, pre_cntr)
    begin
        if(pre_cntr >= cntr_value) then
            next_cntr <= pre_cntr;
            wren <= '0';
        else
            wren <= '1';
            next_cntr <= pre_cntr + '1';
        end if;

        if(rstram = '1') then
            data_out <= (others => '0');
        else
            if(datain = "111111111111110") then -- prevent overflow
                data_out <= datain;
            else
                data_out <= datain + '1';
            end if;
        end if;

        ramwraddr <= wr_addr1;
    end process;

    process(start_cntr, pre_addrcnt) -- generate ramp to clear BRAM
    begin
        next_addrcnt <= pre_addrcnt + '1';
    end process;
end hlsm;