/* C function to set bits in physical byte order */

 #include <stdint.h>

 /* Set a bit in physical memory order
  * bit_pos: which bit to set (0-15 for int16)
  * Physical byte addressing: byte[bit_pos/8], bit within byte[bit_pos%8]
  */
 void set_physical_bit_c(int16_t *value, int bit_pos) {
     unsigned char *byte_ptr = (unsigned char *)value;
     int byte_index = bit_pos / 8;      /* Which byte (0 or 1) */
     int bit_in_byte = bit_pos % 8;     /* Which bit within that byte (0-7) */

     /* Set the bit in physical memory */
     byte_ptr[byte_index] |= (1 << bit_in_byte);
 }

 /* Fortran-callable wrapper */
 void set_physical_bit_c_(int16_t *value, int *bit_pos) {
     set_physical_bit_c(value, *bit_pos);
 }