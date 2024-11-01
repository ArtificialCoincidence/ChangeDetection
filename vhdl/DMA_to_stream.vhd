library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity DMA_to_stream is
    generic (
        IMAGE_WIDTH : integer;  -- Width of the image
        IMAGE_HEIGHT : integer  -- Height of the image
    );
    port (
        clk : in std_logic;                 -- Clock signal
        start : in std_logic;               -- Start signal for the process
        array_image : out std_logic_vector((IMAGE_WIDTH*IMAGE_HEIGHT*8)-1 downto 0)  -- Flattened image data
    );
end DMA_to_stream;

architecture Behavioral of DMA_to_stream is
    -- Define memory to store the image
    type image_mem_type is array (0 to IMAGE_WIDTH*IMAGE_HEIGHT-1) of std_logic_vector(7 downto 0);
    signal image_memory : image_mem_type;
    signal flat_image_signal : std_logic_vector((IMAGE_WIDTH*IMAGE_HEIGHT*8)-1 downto 0);
begin
    process(clk)
        variable idx : integer := 0;  -- Index for flattening
    begin
        if rising_edge(clk) then
            if start = '1' then
                idx := 0;
                -- Loop through each pixel and flatten it into the output array
                for y in 0 to IMAGE_HEIGHT-1 loop
                    for x in 0 to IMAGE_WIDTH-1 loop
                        flat_image_signal((idx+1)*8-1 downto idx*8) <= image_memory(y * IMAGE_WIDTH + x);
                        idx := idx + 1;
                    end loop;
                end loop;
                flat_image <= flat_image_signal;
            end if;
        end if;
    end process;
end Behavioral;