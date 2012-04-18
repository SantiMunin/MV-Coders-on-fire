/**
 * Author: Santiago Munín González
 * Date:   18 April, 2012
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define symbols_length 13

char symbols[symbols_length][2] = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
int values[symbols_length] = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};

/**
 * Calculates the highest magnitude.
 */
int findMagnitude(int decimal_number) {
	int i;
	for (i=0; i<symbols_length; i++) {
		if ((decimal_number / values[i]) > 0) {
			return i;
		}
	}
	return -1;
}

void print_usage() {
		printf("Usage: decimalToRoman number\n");
	
}

int main(int argc, char *argv[]) {
	char result[20] = "";
	int decimal_number, number_magnitude, i, result_index, magnitude;
	
	if (argc != 2) {
		print_usage();
		return(-1);
	}
	
	decimal_number = atoi(argv[1]);
	result_index = 0;
	
	while (decimal_number>0) {
		magnitude = findMagnitude(decimal_number);
		for(i=0; i<decimal_number/values[magnitude] && i<3; i++) {
				if(magnitude%2 == 0) {
					strncpy(&(result[result_index++]),symbols[magnitude],1);
				} else {
					strncpy(&(result[result_index++]),symbols[magnitude],2);
					result_index++;
				}
		}
		if (decimal_number > 1) {
			decimal_number = decimal_number %values[magnitude];
		} else {
			break;
		}
	}
	printf("Result: %s\n", result);
}
