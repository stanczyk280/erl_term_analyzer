#define _TAG_PRIMARY_SIZE	2
#define _TAG_PRIMARY_MASK	0x3
#define TAG_PRIMARY_HEADER	0x0
#define TAG_PRIMARY_LIST	0x1
#define TAG_PRIMARY_BOXED	0x2
#define TAG_PRIMARY_IMMED1	0x3

#define primary_tag(x)	((x) & _TAG_PRIMARY_MASK)

#define _TAG_IMMED1_SIZE	4
#define _TAG_IMMED1_MASK	0xF
#define _TAG_IMMED1_PID		((0x0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_PORT	((0x1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_IMMED2	((0x2 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_SMALL	((0x3 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)

#define _TAG_IMMED2_SIZE	6
#define _TAG_IMMED2_MASK	0x3F
#define _TAG_IMMED2_ATOM	((0x0 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_CATCH	((0x1 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_NIL		((0x3 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)

#define ARITYVAL_SUBTAG         (0x0 << _TAG_PRIMARY_SIZE) /* TUPLE */
#define FUN_REF_SUBTAG          (0x1 << _TAG_PRIMARY_SIZE) /* FUN_REF */
#define _BIG_TAG_MASK           (~(0x1 << _TAG_PRIMARY_SIZE) & _TAG_HEADER_MASK)
#define _BIG_SIGN_BIT           (0x1 << _TAG_PRIMARY_SIZE)
#define POS_BIG_SUBTAG          (0x2 << _TAG_PRIMARY_SIZE) /* BIGNUM */
#define NEG_BIG_SUBTAG          (0x3 << _TAG_PRIMARY_SIZE) /* BIGNUM */
#define REF_SUBTAG              (0x4 << _TAG_PRIMARY_SIZE) /* REF */
#define FUN_SUBTAG              (0x5 << _TAG_PRIMARY_SIZE) /* FUN */
#define FLOAT_SUBTAG            (0x6 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define _BITSTRING_TAG_MASK     (~(0x1 << _TAG_PRIMARY_SIZE) & _TAG_HEADER_MASK)
#define HEAP_BITS_SUBTAG        (0x8 << _TAG_PRIMARY_SIZE) /* BITSTRING */
#define SUB_BITS_SUBTAG         (0x9 << _TAG_PRIMARY_SIZE) /* BITSTRING */
#define BIN_REF_SUBTAG          (0xA << _TAG_PRIMARY_SIZE)
#define MAP_SUBTAG              (0xB << _TAG_PRIMARY_SIZE) /* MAP */
#define _EXTERNAL_TAG_MASK      (~(0x3 << _TAG_PRIMARY_SIZE) & _TAG_HEADER_MASK)
#define EXTERNAL_PID_SUBTAG     (0xC << _TAG_PRIMARY_SIZE) /* EXTERNAL_PID */
#define EXTERNAL_PORT_SUBTAG    (0xD << _TAG_PRIMARY_SIZE) /* EXTERNAL_PORT */
#define EXTERNAL_REF_SUBTAG     (0xE << _TAG_PRIMARY_SIZE) /* EXTERNAL_REF */
/* _EXTERNAL_TAG_MASK requires that 0xF is reserved for external terms. */

#define _TAG_HEADER_ARITYVAL       (TAG_PRIMARY_HEADER|ARITYVAL_SUBTAG)
#define _TAG_HEADER_FUN_REF        (TAG_PRIMARY_HEADER|FUN_REF_SUBTAG)
#define _TAG_HEADER_FUN            (TAG_PRIMARY_HEADER|FUN_SUBTAG)
#define _TAG_HEADER_POS_BIG        (TAG_PRIMARY_HEADER|POS_BIG_SUBTAG)
#define _TAG_HEADER_NEG_BIG        (TAG_PRIMARY_HEADER|NEG_BIG_SUBTAG)
#define _TAG_HEADER_FLOAT          (TAG_PRIMARY_HEADER|FLOAT_SUBTAG)
#define _TAG_HEADER_REF            (TAG_PRIMARY_HEADER|REF_SUBTAG)
#define _TAG_HEADER_BIN_REF        (TAG_PRIMARY_HEADER|BIN_REF_SUBTAG)
#define _TAG_HEADER_HEAP_BITS      (TAG_PRIMARY_HEADER|HEAP_BITS_SUBTAG)
#define _TAG_HEADER_SUB_BITS       (TAG_PRIMARY_HEADER|SUB_BITS_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PID   (TAG_PRIMARY_HEADER|EXTERNAL_PID_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PORT  (TAG_PRIMARY_HEADER|EXTERNAL_PORT_SUBTAG)
#define _TAG_HEADER_EXTERNAL_REF   (TAG_PRIMARY_HEADER|EXTERNAL_REF_SUBTAG)
#define _TAG_HEADER_MAP	           (TAG_PRIMARY_HEADER|MAP_SUBTAG)

#define _TAG_HEADER_MASK	0x3F
#define _HEADER_SUBTAG_MASK	0x3C	/* 4 bits for subtag */
#define _HEADER_ARITY_OFFS	6

#define _TYPE_IMMED1_MASK 0x0C
#define _TYPE_IMMED2_MASK 0x30
#define _VALUE_MASK 0xFFFFFFFFFFFFFFFF