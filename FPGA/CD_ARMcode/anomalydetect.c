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

// Convert from float to fixed-point
inline fixed_point_t float_to_fixed(float input) {
    return (fixed_point_t)(input * (1 << FIXED_POINT_FRACTIONAL_BITS));
}

// Convert from fixed-point back to float
inline float fixed_to_float(fixed_point_t input) {
    return ((float)input / (1 << FIXED_POINT_FRACTIONAL_BITS));
}

typedef float pixel_t; // Define a 16-bit grayscale pixel
int width, height;

// Function to read pixel values from a .txt file
int read_txt(char *filename, fixed_point_t **data) {
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
    *data = (fixed_point_t *)malloc(size * sizeof(fixed_point_t));
    if (*data == NULL) {
        printf("Memory allocation failed\n");
        fclose(file);
        return -1;
    }

    // Read pixel values row by row
    char ch;
    int row = 0, col = 0;
    for (row = 0; row < height; ++row) {
        // Skip to the start of the row
        do {
            if (fscanf(file, "%c", &ch) != 1) {
                printf("Error reading row start for row %d\n", row);
                free(*data);
                fclose(file);
                return -1;
            }
        } while (ch == ' ' || ch == '\n' || ch == '\r');

        if (ch != '<') {
            printf("Error: Expected '<' at the start of row %d\n", row);
            free(*data);
            fclose(file);
            return -1;
        }

        for (col = 0; col < width; ++col) {
            float pixel_value;
            if (fscanf(file, "%f", &pixel_value) != 1) {
                printf("Error reading pixel value at row %d, col %d\n", row, col);
                free(*data);
                fclose(file);
                return -1;
            }

            // Convert float to fixed-point and store it
            (*data)[row * width + col] = float_to_fixed(pixel_value);

            // Skip comma for all but the last column
            if (col < width - 1) {
                if (fscanf(file, "%c", &ch) != 1 || ch != ',') {
                    printf("Error: Expected ',' at row %d, col %d\n", row, col);
                    free(*data);
                    fclose(file);
                    return -1;
                }
            }
        }

        // Ensure the row ends with '>'
        if (fscanf(file, "%c", &ch) != 1 || ch != '>') {
            printf("Error: Expected '>' at the end of row %d\n", row);
            free(*data);
            fclose(file);
            return -1;
        }

        // Skip the comma between rows, except for the last row
        if (row < height - 1) {
            if (fscanf(file, "%c", &ch) != 1 || ch != ',') {
                printf("Error: Expected ',' between rows\n");
                free(*data);
                fclose(file);
                return -1;
            }
        }
    }

    fclose(file);
    return 0;
}

// Function to write pixel values to a .txt file
void write_txt(char *filename, fixed_point_t *data) {
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error opening file for writing: %s\n", filename);
        return;
    }

    // Write width and height as the first line
    fprintf(file, "%d %d\n", width, height);

    // Write pixel values row by row
    float pixel_value
    int i,j;
    for (i = 0; i < height; i++) {
        fprintf(file, "<");
        for (j = 0; j < width; j++) {
            pixel_value = fixed_to_float(data[i * width + j]); // Convert back to float
            //fprintf(file, "%.6f", pixel_value); // Write the value
            if (j < width - 1) {
                fprintf(file, ",");
            }
        }
        fprintf(file, ">");
        if (i < height - 1) {
            fprintf(file, ",");
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
void memcpy_consecutive_to_padded(fixed_point_t *from, volatile unsigned int *to, int pixels) {
    int i;
    for (i = 0; i < pixels; i++) {
        to[i] = from[i]; // Copy 16-bit grayscale data directly
        printf("pixels  %d", i);
    }
}

// Copy word-aligned DMA buffer back to grayscale data
void memcpy_padded_to_consecutive(volatile unsigned int *from, fixed_point_t *to, int pixels) {
    int i;
    for (i = 0; i < pixels; i++) {
        to[i] = (fixed_point_t)(from[i] & 0xFFFF); // Extract 16-bit grayscale value
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

    if ((fd = open_physical(fd)) == -1)
        printf("1");
        return (-1);
    printf("1\n");
    LW_virtual = map_physical(fd, LW_BRIDGE_BASE, LW_BRIDGE_SPAN);
    printf("2\n");
    SDRAM_virtual = map_physical(fd, SDRAM_BASE, SDRAM_SPAN);
    printf("3\n");
    if ((LW_virtual == NULL) || (SDRAM_virtual == NULL))
        return (0);

    mem_to_stream_dma = (volatile unsigned int *)(LW_virtual + 0x3100);
    stream_to_mem_dma = (volatile unsigned int *)(LW_virtual + 0x3120);
    
    mem_to_stream_dma_buffer = (volatile unsigned int *)(SDRAM_virtual);
    stream_to_mem_dma_buffer = (volatile unsigned int *)(SDRAM_virtual + 0x02000000);
    pixel_buffer_dma = (volatile unsigned int *)(LW_virtual + 0x3020);

    *(pixel_buffer_dma + 1) = SDRAM_BASE;
    *(pixel_buffer_dma) = 1;
    while ((*(pixel_buffer_dma + 3)) & 0x1)
        ;
    *(pixel_buffer_dma + 1) = (SDRAM_BASE + 0x02000000);
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
