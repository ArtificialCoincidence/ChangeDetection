#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "address_map_arm.h"
#include "physical.h"

#define FIXED_POINT_FRACTIONAL_BITS 15
typedef int16_t fixed_point_t;
typedef float pixel_t; // Define a 16-bit grayscale pixel
int width, height;


inline fixed_point_t double_to_fixed(double input)
{
    return (fixed_point_t)((input * (1 << FIXED_POINT_FRACTIONAL_BITS)));
}

inline double fixed_to_double(fixed_point_t input)
{
    return ((double)input / (double)(1 << FIXED_POINT_FRACTIONAL_BITS));
}
// Function to read pixel values from a .txt file
int read_txt(char *filename, pixel_t **data) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        printf("Error opening file: %s\n", filename);
        return -1;
    }

    // Read width and height from the first line
    if (fscanf(file, "%d %d", &width, &height) != 2) {
        printf("Error reading dimensions from file\n");
        fclose(file);
        return -1;
    }
    printf("Width: %d, Height: %d\n", width, height);

    // Allocate memory for pixel data
    int size = width * height;
    *data = (pixel_t *)malloc(size * sizeof(pixel_t));
    if (*data == NULL) {
        printf("Memory allocation failed\n");
        fclose(file);
        return -1;
    }

    // Read pixel values row by row
    pixel_t *pixel_ptr = *data; // Pointer to traverse the pixel array
    char ch;
    int row = 0, col = 0;
    // Loop through the file and read data row by row
    
    for (row = 0; row < height; ++row) {
        while (fscanf(file, "%c", &ch) == 1 && (ch == ' ' || ch == '\n' || ch == '\r')) {
            // Skipping any spaces, newlines, or carriage returns
        }

        // Ensure '<' exists at the start of the row
        if (ch != '<') {
            printf("Error: Expected '<' at the start of row %d, but got '%c'.\n", row, ch);
            free(*data);
            fclose(file);
            return -1;
        }


        // Read pixels in the row
        for (col = 0; col < width; ++col) {
            float pixel_value;
            if (fscanf(file, "%f", &pixel_value) != 1) {
                printf("Error: Invalid pixel value at row %d, col %d.\n", row, col);
                free(*data);
                fclose(file);
                return -1;
            }

            fixed_point_t new_pixel;
            new_pixel = double_to_fixed(pixel_value);
            pixel_ptr[row * width + col] = (pixel_t) pixel_value;

            // If not the last column, skip the comma
            if (col < width - 1 && fscanf(file, ",") != 0) {
                printf("Error: Expected ',' between pixel values.\n");
                free(*data);
                fclose(file);
                return -1;
            }
        }

        // Skip the closing '>'
        if (fscanf(file, ">") != 0) {
            printf("Error: Expected '>' at the end of a row.\n");
            free(*data);
            fclose(file);
            return -1;
        }

        // Skip the comma between rows (but not for the last row)
        if (row < height - 1 && fscanf(file, ",") != 0) {
            printf("Error: Expected ',' between rows.\n");
            free(*data);
            fclose(file);
            return -1;
        }
    }

    fclose(file);
    return 0;
}

// Function to write pixel values to a .txt file
void write_txt(char *filename, pixel_t *data) {
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error opening file for writing: %s\n", filename);
        return;
    }

    // Write width and height as the first line
    fprintf(file, "%d %d\n", width, height);

    double new_data;
    new_data = fixed_to_double(data);
    // Write pixel values row by row
    int i,j;
    for (i = 0; i < height; i++) {
        for (j = 0; j < width; j++) {
            fprintf(file, "%f ", data[i * width + j]);
        }
        fprintf(file, "\n");
    }

    fclose(file);
}

// Flip the grayscale image vertically
void flip(pixel_t *data, int width, int height) {
    int i, j;
    pixel_t tmp;
    pixel_t (*image)[width] = (pixel_t(*)[width])data;

    for (i = 0; i < height / 2; ++i) {
        for (j = 0; j < width; ++j) {
            tmp = image[i][j];
            image[i][j] = image[(height - 1) - i][j];
            image[(height - 1) - i][j] = tmp;
        }
    }
}

// Copy grayscale data to word-aligned DMA buffer
void memcpy_consecutive_to_padded(pixel_t *from, volatile unsigned int *to, int pixels) {
    int i;
    for (i = 0; i < pixels; i++) {
        to[i] = from[i]; // Copy 16-bit grayscale data directly
        printf("pixels  %d", i);
    }
}

// Copy word-aligned DMA buffer back to grayscale data
void memcpy_padded_to_consecutive(volatile unsigned int *from, pixel_t *to, int pixels) {
    int i;
    for (i = 0; i < pixels; i++) {
        to[i] = from[i] & 0xFFFF; // Extract 16-bit grayscale value
    }
}

int main(int argc, char *argv[]) {
    pixel_t *data = NULL;
    int fd = -1;
    void *LW_virtual;
    void *SDRAM_virtual;
    time_t start, end;

    volatile unsigned int *mem_to_stream_dma = NULL;
    volatile unsigned int *stream_to_mem_dma = NULL;
    volatile unsigned int *pixel_buffer_dma = NULL;

    volatile unsigned int *mem_to_stream_dma_buffer = NULL;
    volatile unsigned int *stream_to_mem_dma_buffer = NULL;

    if (argc < 2) {
        printf("Usage: edgedetect <TXT filename>\n");
        return 0;
    }

    printf("%s", argv[1]);
    if (read_txt(argv[1], &data) < 0) {
        printf("Failed to read TXT file\n");
        return 0;
    }
    printf("Image width = %d pixels, Image height = %d pixels\n", width, height);

    //write_txt("edges.txt", data);
    printf("0\n");
    if ((fd = open_physical(fd)) == -1)
        printf("1");
        return (-1);
    printf("1\n");
    LW_virtual = map_physical(fd, LW_BRIDGE_BASE, LW_BRIDGE_SPAN);
    printf("2\n");
    SDRAM_virtual = map_physical(fd, SDRAM_BASE, SDRAM_SPAN);
    printf("3\n");
    if ((LW_virtual == NULL) || (SDRAM_virtual == NULL))
        //printf("4");
        return (0);

    mem_to_stream_dma = (volatile unsigned int *)(LW_virtual + 0x3100);
    //printf("5");
    stream_to_mem_dma = (volatile unsigned int *)(LW_virtual + 0x3120);
    //printf("6");
    mem_to_stream_dma_buffer = (volatile unsigned int *)(SDRAM_virtual);
    //printf("7");
    stream_to_mem_dma_buffer = (volatile unsigned int *)(SDRAM_virtual + 0x02000000);
    //printf("8");
    pixel_buffer_dma = (volatile unsigned int *)(LW_virtual + 0x3020);
    //printf("9");

    *(pixel_buffer_dma + 1) = SDRAM_BASE;
    //printf("10");
    *(pixel_buffer_dma) = 1;
    //printf("11");
    while ((*(pixel_buffer_dma + 3)) & 0x1)
        ;
    *(pixel_buffer_dma + 1) = (SDRAM_BASE + 0x02000000);
    //printf("12");
    start = clock();

    *(mem_to_stream_dma + 3) = 0;
    *(stream_to_mem_dma + 3) = 0;

    memcpy_consecutive_to_padded(data, mem_to_stream_dma_buffer, width * height);

    printf("Press return to continue");
    getchar();
    *(stream_to_mem_dma + 3) = 4;
    *(mem_to_stream_dma + 3) = 4;

    *(mem_to_stream_dma) = 1;
    while ((*(mem_to_stream_dma + 3)) & 0x1)
        ;
    *(stream_to_mem_dma) = 1;
    while ((*(stream_to_mem_dma + 3)) & 0x1)
        ;

    *(mem_to_stream_dma + 3) = 0;
    *(stream_to_mem_dma + 3) = 0;

    memcpy_padded_to_consecutive(stream_to_mem_dma_buffer, data, width * height);
    end = clock();

    *(pixel_buffer_dma) = 1;
    while ((*(pixel_buffer_dma + 3)) & 0x1)
        ;

    printf("TIME ELAPSED: %.0f ms\n", ((double)(end - start)) * 1000 / CLOCKS_PER_SEC);

    write_txt("edges.txt", data);

    free(data);

    unmap_physical(LW_virtual, LW_BRIDGE_SPAN);
    unmap_physical(SDRAM_virtual, SDRAM_SPAN);
    close_physical(fd);

    return 0;
}
