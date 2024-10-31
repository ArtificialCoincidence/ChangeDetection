library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;  
use work.types.all;                                                                                           

entity avg_filter is
    generic(
        V_SIZE: integer;    -- vertical size image
        H_SIZE: integer;    -- horizontal size image
        K_SIZE: integer);   -- size of the kernel
    
    port(
        clk         : in std_logic;
        rst         : in std_logic;
        start       : in std_logic;
        data_in     : in ufixed(-1 downto -32);

        --eoi       : out std_logic   -- end of image
        busy        : out std_logic;
        o_valid     : out std_logic;    -- valid after filling kernel
        avg_out     : out ufixed(-1 downto -32)
        );   
end avg_filter;

architecture rtl of avg_filter is

    component controller
        generic(
            V_SIZE: integer;
            H_SIZE: integer;
            K_SIZE: integer
        );
        port(
            clk         : in std_logic;
            rst         : in std_logic;
            start       : in std_logic;
            --eo_image    : out std_logic;
            busy        : out std_logic;
            o_valid     : out std_logic;
            shift       : out std_logic;
            conv        : out std_logic
        );
    end component;

    component convolution is
        generic(
            V_SIZE: integer;
            H_SIZE: integer;
            K_SIZE: integer
        );
        port(
            clk         : in std_logic;
            rst         : in std_logic;
            shift       : in std_logic;
            conv        : in std_logic;
            data_in     : in ufixed(-1 downto -32);
            avg_out     : out ufixed(-1 downto -32)
        );
    end component;

    signal shift_f: std_logic;
    signal conv_f: std_logic;

begin

    CNTR: controller
    generic map(V_SIZE, H_SIZE, K_SIZE)
    port map(clk, rst, start, busy, o_valid, shift_f, conv_f);
    
    CONV: convolution
    generic map(V_SIZE, H_SIZE, K_SIZE)
    port map(clk, rst, shift_f, conv_f, data_in, avg_out);

end rtl;
    
    -- control: process (clk, rst)
    --     variable counter : natural range K_SIZE**2 downto 0 := 0;
    -- begin
    --     if rst = '1' then
    --         fill <= '0';
    --         compute <= '0';
    --         r_valid <= '0';
    --         o_valid <= '0';
    --         counter := 0;
    --     elsif rising_edge(clk) then
    --         r_valid <= i_data_en;
    --         o_valid <= r_valid;

    --         counter := counter + 1;

    --         if counter < K_SIZE**2 then
    --             compute <= '0';
    --             if (counter mod H_SIZE) < K_SIZE then
    --                 fill <= '1';
    --             else
    --                 fill <= '0';
    --             end if;
    --         else
    --             compute <= '1';
    --         end if;
    --     end if;
    -- end process;

    -- shifter_handler: process (clk, rst)
    -- begin
    --     if rst = '1' then
    --         matrix_line <= (others => (others => '0'));
    --     elsif rising_edge(clk) then
    --         if i_data_en = '1' then 
    --             matrix_line(IM_BUF-1 downto 0) <= matrix_line(IM_BUF-2 downto 0) & to_ufixed(data_in, -1, -32);
    --         end if;
    --     end if;
    -- end process;

    -- kernel_handler: process (clk, rst)
    -- begin
    --     if rst = '1' then
    --         kernel1 <= (others => (others => '0'));
    --         kernelk <= (others => (others => '0'));
    --     elsif rising_edge(clk) then
    --         for i in 0 to K_SIZE-1 loop
    --             kernel1(i) <= matrix_line(IM_BUF-i*H_SIZE-1);
    --             kernelk(i) <= matrix_line(IM_BUF-i*H_SIZE-K_SIZE);
    --         end loop;
    --     end if;
    -- end process;

    -- accumulator: process (clk, rst)
    --     variable sum_k1     : ufixed(4 downto -32);
    --     variable sum_kk     : ufixed(4 downto -32);
    --     variable r_acc      : ufixed(6 downto -32);  -- HOW MANY BITS????
    --     variable moving_avg : ufixed(-1 downto -32);
    -- begin
    --     if rst = '1' then
    --         r_acc := (others => '0');
    --         moving_avg := (others => '0');
    --     elsif rising_edge(clk) then
    --         for i in 0 to K_SIZE-1 loop
    --             sum_k1 := resize(sum_k1, 3, -32) + kernel1(i);  -- HOW MANY BITS????
    --             sum_kk := resize(sum_kk, 3, -32) + kernelk(i);
    --         end loop;

    --         if i_data_en = '1' then
    --             if fill = '1' then
    --                 r_acc := resize(r_acc, 5, -32) + to_ufixed(data_in, -1, -32);
    --             elsif compute = '1' then
    --                 r_acc := resize(r_acc, 5, -32) - sum_k1 + sum_kk;  -- HOW MANY BITS????
    --                 moving_avg := r_acc/K_SIZE**2;
    --             end if;
    --         end if;

    --         data_out <= moving_avg;

    --     end if;
    -- end process;
-- end rtl;