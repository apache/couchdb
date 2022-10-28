/*
 * Hash Table Data Type
 * Copyright (C) 1997 Kaz Kylheku <kaz@ashi.footprints.net>
 *
 * Free Software License:
 *
 * All rights are reserved by the author, with the following exceptions:
 * Permission is granted to freely reproduce and distribute this software,
 * possibly in exchange for a fee, provided that this copyright notice appears
 * intact. Permission is also granted to adapt this software to produce
 * derivative works, as long as the modified versions carry this copyright
 * notice and additional notices stating that the work has been modified.
 * This source code may be translated into executable form and incorporated
 * into proprietary software; there is no requirement for such software to
 * contain a copyright notice related to this source.
 *
 * $Id: hash.c,v 1.36.2.11 2000/11/13 01:36:45 kaz Exp $
 * $Name: kazlib_1_20 $
 */

#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#define HASH_IMPLEMENTATION
#include "hash.h"

#ifdef KAZLIB_RCSID
static const char rcsid[] = "$Id: hash.c,v 1.36.2.11 2000/11/13 01:36:45 kaz Exp $";
#endif

#define INIT_BITS	6
#define INIT_SIZE	(1UL << (INIT_BITS))	/* must be power of two		*/
#define INIT_MASK	((INIT_SIZE) - 1)

#define next hash_next
#define key hash_key
#define data hash_data
#define hkey hash_hkey

#define table hash_table
#define nchains hash_nchains
#define nodecount hash_nodecount
#define maxcount hash_maxcount
#define highmark hash_highmark
#define lowmark hash_lowmark
#define compare hash_compare
#define function hash_function
#define allocnode hash_allocnode
#define freenode hash_freenode
#define context hash_context
#define mask hash_mask
#define dynamic hash_dynamic

#define table hash_table
#define chain hash_chain

static hnode_t *kl_hnode_alloc(void *context);
static void kl_hnode_free(hnode_t *node, void *context);
static hash_val_t hash_fun_default(const void *key);
static int hash_comp_default(const void *key1, const void *key2);

int hash_val_t_bit;

/*
 * Compute the number of bits in the hash_val_t type.  We know that hash_val_t
 * is an unsigned integral type. Thus the highest value it can hold is a
 * Mersenne number (power of two, less one). We initialize a hash_val_t
 * object with this value and then shift bits out one by one while counting.
 * Notes:
 * 1. HASH_VAL_T_MAX is a Mersenne number---one that is one less than a power
 *    of two. This means that its binary representation consists of all one
 *    bits, and hence ``val'' is initialized to all one bits.
 * 2. While bits remain in val, we increment the bit count and shift it to the
 *    right, replacing the topmost bit by zero.
 */

static void compute_bits(void)
{
    hash_val_t val = HASH_VAL_T_MAX;	/* 1 */
    int bits = 0;

    while (val) {	/* 2 */
	bits++;
	val >>= 1;
    }

    hash_val_t_bit = bits;
}

/*
 * Verify whether the given argument is a power of two.
 */

static int is_power_of_two(hash_val_t arg)
{
    if (arg == 0)
	return 0;
    while ((arg & 1) == 0)
	arg >>= 1;
    return (arg == 1);
}

/*
 * Compute a shift amount from a given table size 
 */

static hash_val_t compute_mask(hashcount_t size)
{
    assert (is_power_of_two(size));
    assert (size >= 2);

    return size - 1;
}

/*
 * Initialize the table of pointers to null.
 */

static void clear_table(hash_t *hash)
{
    hash_val_t i;

    for (i = 0; i < hash->nchains; i++)
	hash->table[i] = NULL;
}

/*
 * Double the size of a dynamic table. This works as follows. Each chain splits
 * into two adjacent chains.  The shift amount increases by one, exposing an
 * additional bit of each hashed key. For each node in the original chain, the
 * value of this newly exposed bit will decide which of the two new chains will
 * receive the node: if the bit is 1, the chain with the higher index will have
 * the node, otherwise the lower chain will receive the node. In this manner,
 * the hash table will continue to function exactly as before without having to
 * rehash any of the keys.
 * Notes:
 * 1.  Overflow check.
 * 2.  The new number of chains is twice the old number of chains.
 * 3.  The new mask is one bit wider than the previous, revealing a
 *     new bit in all hashed keys.
 * 4.  Allocate a new table of chain pointers that is twice as large as the
 *     previous one.
 * 5.  If the reallocation was successful, we perform the rest of the growth
 *     algorithm, otherwise we do nothing.
 * 6.  The exposed_bit variable holds a mask with which each hashed key can be
 *     AND-ed to test the value of its newly exposed bit.
 * 7.  Now loop over each chain in the table and sort its nodes into two
 *     chains based on the value of each node's newly exposed hash bit.
 * 8.  The low chain replaces the current chain.  The high chain goes
 *     into the corresponding sister chain in the upper half of the table.
 * 9.  We have finished dealing with the chains and nodes. We now update
 *     the various bookeeping fields of the hash structure.
 */

static void grow_table(hash_t *hash)
{
    hnode_t **newtable;

    assert (2 * hash->nchains > hash->nchains);	/* 1 */

    newtable = realloc(hash->table,
	    sizeof *newtable * hash->nchains * 2);	/* 4 */

    if (newtable) {	/* 5 */
	hash_val_t mask = (hash->mask << 1) | 1;	/* 3 */
	hash_val_t exposed_bit = mask ^ hash->mask;	/* 6 */
	hash_val_t chain;

	assert (mask != hash->mask);

	for (chain = 0; chain < hash->nchains; chain++) { /* 7 */
	    hnode_t *low_chain = 0, *high_chain = 0, *hptr, *next;

	    for (hptr = newtable[chain]; hptr != 0; hptr = next) {
		next = hptr->next;

		if (hptr->hkey & exposed_bit) {
		    hptr->next = high_chain;
		    high_chain = hptr;
		} else {
		    hptr->next = low_chain;
		    low_chain = hptr;
		}
	    }

	    newtable[chain] = low_chain; 	/* 8 */
	    newtable[chain + hash->nchains] = high_chain;
	}

	hash->table = newtable;			/* 9 */
	hash->mask = mask;
	hash->nchains *= 2;
	hash->lowmark *= 2;
	hash->highmark *= 2;
    }
    assert (kl_hash_verify(hash));
}

/*
 * Cut a table size in half. This is done by folding together adjacent chains
 * and populating the lower half of the table with these chains. The chains are
 * simply spliced together. Once this is done, the whole table is reallocated
 * to a smaller object.
 * Notes:
 * 1.  It is illegal to have a hash table with one slot. This would mean that
 *     hash->shift is equal to hash_val_t_bit, an illegal shift value.
 *     Also, other things could go wrong, such as hash->lowmark becoming zero.
 * 2.  Looping over each pair of sister chains, the low_chain is set to
 *     point to the head node of the chain in the lower half of the table, 
 *     and high_chain points to the head node of the sister in the upper half.
 * 3.  The intent here is to compute a pointer to the last node of the
 *     lower chain into the low_tail variable. If this chain is empty,
 *     low_tail ends up with a null value.
 * 4.  If the lower chain is not empty, we simply tack the upper chain onto it.
 *     If the upper chain is a null pointer, nothing happens.
 * 5.  Otherwise if the lower chain is empty but the upper one is not,
 *     If the low chain is empty, but the high chain is not, then the
 *     high chain is simply transferred to the lower half of the table.
 * 6.  Otherwise if both chains are empty, there is nothing to do.
 * 7.  All the chain pointers are in the lower half of the table now, so
 *     we reallocate it to a smaller object. This, of course, invalidates
 *     all pointer-to-pointers which reference into the table from the
 *     first node of each chain.
 * 8.  Though it's unlikely, the reallocation may fail. In this case we
 *     pretend that the table _was_ reallocated to a smaller object.
 * 9.  Finally, update the various table parameters to reflect the new size.
 */

static void shrink_table(hash_t *hash)
{
    hash_val_t chain, nchains;
    hnode_t **newtable, *low_tail, *low_chain, *high_chain;

    assert (hash->nchains >= 2);			/* 1 */
    nchains = hash->nchains / 2;

    for (chain = 0; chain < nchains; chain++) {
	low_chain = hash->table[chain];		/* 2 */
	high_chain = hash->table[chain + nchains];
	for (low_tail = low_chain; low_tail && low_tail->next; low_tail = low_tail->next)
	    ;	/* 3 */
	if (low_chain != 0)				/* 4 */
	    low_tail->next = high_chain;
	else if (high_chain != 0)			/* 5 */
	    hash->table[chain] = high_chain;
	else
	    assert (hash->table[chain] == NULL);	/* 6 */
    }
    newtable = realloc(hash->table,
	    sizeof *newtable * nchains);		/* 7 */
    if (newtable)					/* 8 */
	hash->table = newtable;
    hash->mask >>= 1;			/* 9 */
    hash->nchains = nchains;
    hash->lowmark /= 2;
    hash->highmark /= 2;
    assert (kl_hash_verify(hash));
}


/*
 * Create a dynamic hash table. Both the hash table structure and the table
 * itself are dynamically allocated. Furthermore, the table is extendible in
 * that it will automatically grow as its load factor increases beyond a
 * certain threshold.
 * Notes:
 * 1. If the number of bits in the hash_val_t type has not been computed yet,
 *    we do so here, because this is likely to be the first function that the
 *    user calls.
 * 2. Allocate a hash table control structure.
 * 3. If a hash table control structure is successfully allocated, we
 *    proceed to initialize it. Otherwise we return a null pointer.
 * 4. We try to allocate the table of hash chains.
 * 5. If we were able to allocate the hash chain table, we can finish
 *    initializing the hash structure and the table. Otherwise, we must
 *    backtrack by freeing the hash structure.
 * 6. INIT_SIZE should be a power of two. The high and low marks are always set
 *    to be twice the table size and half the table size respectively. When the
 *    number of nodes in the table grows beyond the high size (beyond load
 *    factor 2), it will double in size to cut the load factor down to about
 *    about 1. If the table shrinks down to or beneath load factor 0.5,
 *    it will shrink, bringing the load up to about 1. However, the table
 *    will never shrink beneath INIT_SIZE even if it's emptied.
 * 7. This indicates that the table is dynamically allocated and dynamically
 *    resized on the fly. A table that has this value set to zero is
 *    assumed to be statically allocated and will not be resized.
 * 8. The table of chains must be properly reset to all null pointers.
 */

hash_t *kl_hash_create(hashcount_t maxcount, hash_comp_t compfun,
	hash_fun_t hashfun)
{
    hash_t *hash;

    if (hash_val_t_bit == 0)	/* 1 */
	compute_bits();

    hash = malloc(sizeof *hash);	/* 2 */

    if (hash) {		/* 3 */
	hash->table = malloc(sizeof *hash->table * INIT_SIZE);	/* 4 */
	if (hash->table) {	/* 5 */
	    hash->nchains = INIT_SIZE;		/* 6 */
	    hash->highmark = INIT_SIZE * 2;
	    hash->lowmark = INIT_SIZE / 2;
	    hash->nodecount = 0;
	    hash->maxcount = maxcount;
	    hash->compare = compfun ? compfun : hash_comp_default;
	    hash->function = hashfun ? hashfun : hash_fun_default;
	    hash->allocnode = kl_hnode_alloc;
	    hash->freenode = kl_hnode_free;
	    hash->context = NULL;
	    hash->mask = INIT_MASK;
	    hash->dynamic = 1;			/* 7 */
	    clear_table(hash);			/* 8 */
	    assert (kl_hash_verify(hash));
	    return hash;
	} 
	free(hash);
    }

    return NULL;
}

/*
 * Select a different set of node allocator routines.
 */

void kl_hash_set_allocator(hash_t *hash, hnode_alloc_t al,
	hnode_free_t fr, void *context)
{
    assert (kl_hash_count(hash) == 0);
    assert ((al == 0 && fr == 0) || (al != 0 && fr != 0));

    hash->allocnode = al ? al : kl_hnode_alloc;
    hash->freenode = fr ? fr : kl_hnode_free;
    hash->context = context;
}

/*
 * Free every node in the hash using the hash->freenode() function pointer, and
 * cause the hash to become empty.
 */

void kl_hash_free_nodes(hash_t *hash)
{
    hscan_t hs;
    hnode_t *node;
    kl_hash_scan_begin(&hs, hash);
    while ((node = kl_hash_scan_next(&hs))) {
	kl_hash_scan_delete(hash, node);
	hash->freenode(node, hash->context);
    }
    hash->nodecount = 0;
    clear_table(hash);
}

/*
 * Obsolescent function for removing all nodes from a table,
 * freeing them and then freeing the table all in one step.
 */

void kl_hash_free(hash_t *hash)
{
#ifdef KAZLIB_OBSOLESCENT_DEBUG
    assert ("call to obsolescent function hash_free()" && 0);
#endif
    kl_hash_free_nodes(hash);
    kl_hash_destroy(hash);
}

/*
 * Free a dynamic hash table structure.
 */

void kl_hash_destroy(hash_t *hash)
{
    assert (hash_val_t_bit != 0);
    assert (kl_hash_isempty(hash));
    free(hash->table);
    free(hash);
}

/*
 * Initialize a user supplied hash structure. The user also supplies a table of
 * chains which is assigned to the hash structure. The table is static---it
 * will not grow or shrink.
 * 1. See note 1. in hash_create().
 * 2. The user supplied array of pointers hopefully contains nchains nodes.
 * 3. See note 7. in hash_create().
 * 4. We must dynamically compute the mask from the given power of two table
 *    size. 
 * 5. The user supplied table can't be assumed to contain null pointers,
 *    so we reset it here.
 */

hash_t *kl_hash_init(hash_t *hash, hashcount_t maxcount,
	hash_comp_t compfun, hash_fun_t hashfun, hnode_t **table,
	hashcount_t nchains)
{
    if (hash_val_t_bit == 0)	/* 1 */
	compute_bits();

    assert (is_power_of_two(nchains));

    hash->table = table;	/* 2 */
    hash->nchains = nchains;
    hash->nodecount = 0;
    hash->maxcount = maxcount;
    hash->compare = compfun ? compfun : hash_comp_default;
    hash->function = hashfun ? hashfun : hash_fun_default;
    hash->dynamic = 0;		/* 3 */
    hash->mask = compute_mask(nchains);	/* 4 */
    clear_table(hash);		/* 5 */

    assert (kl_hash_verify(hash));

    return hash;
}

/*
 * Reset the hash scanner so that the next element retrieved by
 * hash_scan_next() shall be the first element on the first non-empty chain. 
 * Notes:
 * 1. Locate the first non empty chain.
 * 2. If an empty chain is found, remember which one it is and set the next
 *    pointer to refer to its first element.
 * 3. Otherwise if a chain is not found, set the next pointer to NULL
 *    so that hash_scan_next() shall indicate failure.
 */

void kl_hash_scan_begin(hscan_t *scan, hash_t *hash)
{
    hash_val_t nchains = hash->nchains;
    hash_val_t chain;

    scan->table = hash;

    /* 1 */

    for (chain = 0; chain < nchains && hash->table[chain] == 0; chain++)
	;

    if (chain < nchains) {	/* 2 */
	scan->chain = chain;
	scan->next = hash->table[chain];
    } else {			/* 3 */
	scan->next = NULL;
    }
}

/*
 * Retrieve the next node from the hash table, and update the pointer
 * for the next invocation of hash_scan_next(). 
 * Notes:
 * 1. Remember the next pointer in a temporary value so that it can be
 *    returned.
 * 2. This assertion essentially checks whether the module has been properly
 *    initialized. The first point of interaction with the module should be
 *    either hash_create() or hash_init(), both of which set hash_val_t_bit to
 *    a non zero value.
 * 3. If the next pointer we are returning is not NULL, then the user is
 *    allowed to call hash_scan_next() again. We prepare the new next pointer
 *    for that call right now. That way the user is allowed to delete the node
 *    we are about to return, since we will no longer be needing it to locate
 *    the next node.
 * 4. If there is a next node in the chain (next->next), then that becomes the
 *    new next node, otherwise ...
 * 5. We have exhausted the current chain, and must locate the next subsequent
 *    non-empty chain in the table.
 * 6. If a non-empty chain is found, the first element of that chain becomes
 *    the new next node. Otherwise there is no new next node and we set the
 *    pointer to NULL so that the next time hash_scan_next() is called, a null
 *    pointer shall be immediately returned.
 */


hnode_t *kl_hash_scan_next(hscan_t *scan)
{
    hnode_t *next = scan->next;		/* 1 */
    hash_t *hash = scan->table;
    hash_val_t chain = scan->chain + 1;
    hash_val_t nchains = hash->nchains;

    assert (hash_val_t_bit != 0);	/* 2 */

    if (next) {			/* 3 */
	if (next->next) {	/* 4 */
	    scan->next = next->next;
	} else {
	    while (chain < nchains && hash->table[chain] == 0)	/* 5 */
	    	chain++;
	    if (chain < nchains) {	/* 6 */
		scan->chain = chain;
		scan->next = hash->table[chain];
	    } else {
		scan->next = NULL;
	    }
	}
    }
    return next;
}

/*
 * Insert a node into the hash table.
 * Notes:
 * 1. It's illegal to insert more than the maximum number of nodes. The client
 *    should verify that the hash table is not full before attempting an
 *    insertion.
 * 2. The same key may not be inserted into a table twice.
 * 3. If the table is dynamic and the load factor is already at >= 2,
 *    grow the table.
 * 4. We take the bottom N bits of the hash value to derive the chain index,
 *    where N is the base 2 logarithm of the size of the hash table. 
 */

void kl_hash_insert(hash_t *hash, hnode_t *node, const void *key)
{
    hash_val_t hkey, chain;

    assert (hash_val_t_bit != 0);
    assert (node->next == NULL);
    assert (hash->nodecount < hash->maxcount);	/* 1 */
    assert (kl_hash_lookup(hash, key) == NULL);	/* 2 */

    if (hash->dynamic && hash->nodecount >= hash->highmark)	/* 3 */
	grow_table(hash);

    hkey = hash->function(key);
    chain = hkey & hash->mask;	/* 4 */

    node->key = key;
    node->hkey = hkey;
    node->next = hash->table[chain];
    hash->table[chain] = node;
    hash->nodecount++;

    assert (kl_hash_verify(hash));
}

/*
 * Find a node in the hash table and return a pointer to it.
 * Notes:
 * 1. We hash the key and keep the entire hash value. As an optimization, when
 *    we descend down the chain, we can compare hash values first and only if
 *    hash values match do we perform a full key comparison. 
 * 2. To locate the chain from among 2^N chains, we look at the lower N bits of
 *    the hash value by anding them with the current mask.
 * 3. Looping through the chain, we compare the stored hash value inside each
 *    node against our computed hash. If they match, then we do a full
 *    comparison between the unhashed keys. If these match, we have located the
 *    entry.
 */

hnode_t *kl_hash_lookup(hash_t *hash, const void *key)
{
    hash_val_t hkey, chain;
    hnode_t *nptr;

    hkey = hash->function(key);		/* 1 */
    chain = hkey & hash->mask;		/* 2 */

    for (nptr = hash->table[chain]; nptr; nptr = nptr->next) {	/* 3 */
	if (nptr->hkey == hkey && hash->compare(nptr->key, key) == 0)
	    return nptr;
    }

    return NULL;
}

/*
 * Delete the given node from the hash table.  Since the chains
 * are singly linked, we must locate the start of the node's chain
 * and traverse.
 * Notes:
 * 1. The node must belong to this hash table, and its key must not have
 *    been tampered with.
 * 2. If this deletion will take the node count below the low mark, we
 *    shrink the table now. 
 * 3. Determine which chain the node belongs to, and fetch the pointer
 *    to the first node in this chain.
 * 4. If the node being deleted is the first node in the chain, then
 *    simply update the chain head pointer.
 * 5. Otherwise advance to the node's predecessor, and splice out
 *    by updating the predecessor's next pointer.
 * 6. Indicate that the node is no longer in a hash table.
 */

hnode_t *kl_hash_delete(hash_t *hash, hnode_t *node)
{
    hash_val_t chain;
    hnode_t *hptr;

    assert (kl_hash_lookup(hash, node->key) == node);	/* 1 */
    assert (hash_val_t_bit != 0);

    if (hash->dynamic && hash->nodecount <= hash->lowmark
	    && hash->nodecount > INIT_SIZE)
	shrink_table(hash);				/* 2 */

    chain = node->hkey & hash->mask;			/* 3 */
    hptr = hash->table[chain];

    if (hptr == node) {					/* 4 */
	hash->table[chain] = node->next;
    } else {
	while (hptr->next != node) {			/* 5 */
	    assert (hptr != 0);
	    hptr = hptr->next;
	}
	assert (hptr->next == node);
	hptr->next = node->next;
    }
	
    hash->nodecount--;
    assert (kl_hash_verify(hash));

    node->next = NULL;					/* 6 */
    return node;
}

int kl_hash_alloc_insert(hash_t *hash, const void *key, void *data)
{
    hnode_t *node = hash->allocnode(hash->context);

    if (node) {
	kl_hnode_init(node, data);
	kl_hash_insert(hash, node, key);
	return 1;
    }
    return 0;
}

void kl_hash_delete_free(hash_t *hash, hnode_t *node)
{
    kl_hash_delete(hash, node);
    hash->freenode(node, hash->context);
}

/*
 *  Exactly like hash_delete, except does not trigger table shrinkage. This is to be
 *  used from within a hash table scan operation. See notes for hash_delete.
 */

hnode_t *kl_hash_scan_delete(hash_t *hash, hnode_t *node)
{
    hash_val_t chain;
    hnode_t *hptr;

    assert (kl_hash_lookup(hash, node->key) == node);
    assert (hash_val_t_bit != 0);

    chain = node->hkey & hash->mask;
    hptr = hash->table[chain];

    if (hptr == node) {
	hash->table[chain] = node->next;
    } else {
	while (hptr->next != node) 
	    hptr = hptr->next;
	hptr->next = node->next;
    }
	
    hash->nodecount--;
    assert (kl_hash_verify(hash));
    node->next = NULL;

    return node;
}

/*
 * Like hash_delete_free but based on hash_scan_delete.
 */

void kl_hash_scan_delfree(hash_t *hash, hnode_t *node)
{
    kl_hash_scan_delete(hash, node);
    hash->freenode(node, hash->context);
}

/*
 * Verify whether the given object is a valid hash table. This means
 * Notes:
 * 1. If the hash table is dynamic, verify whether the high and
 *    low expansion/shrinkage thresholds are powers of two.
 * 2. Count all nodes in the table, and test each hash value
 *    to see whether it is correct for the node's chain.
 */

int kl_hash_verify(hash_t *hash)
{
    hashcount_t count = 0;
    hash_val_t chain;
    hnode_t *hptr;

    if (hash->dynamic) {	/* 1 */
	if (hash->lowmark >= hash->highmark)
	    return 0;
	if (!is_power_of_two(hash->highmark))
	    return 0;
	if (!is_power_of_two(hash->lowmark))
	    return 0;
    }

    for (chain = 0; chain < hash->nchains; chain++) {	/* 2 */
	for (hptr = hash->table[chain]; hptr != 0; hptr = hptr->next) {
	    if ((hptr->hkey & hash->mask) != chain)
		return 0;
	    count++;
	}
    }

    if (count != hash->nodecount)
	return 0;

    return 1;
}

/*
 * Test whether the hash table is full and return 1 if this is true,
 * 0 if it is false.
 */

#undef kl_hash_isfull
int kl_hash_isfull(hash_t *hash)
{
    return hash->nodecount == hash->maxcount;
}

/*
 * Test whether the hash table is empty and return 1 if this is true,
 * 0 if it is false.
 */

#undef kl_hash_isempty
int kl_hash_isempty(hash_t *hash)
{
    return hash->nodecount == 0;
}

static hnode_t *kl_hnode_alloc(void *context)
{
    return malloc(sizeof *kl_hnode_alloc(NULL));
}

static void kl_hnode_free(hnode_t *node, void *context)
{
    free(node);
}


/*
 * Create a hash table node dynamically and assign it the given data.
 */

hnode_t *kl_hnode_create(void *data)
{
    hnode_t *node = malloc(sizeof *node);
    if (node) {
	node->data = data;
	node->next = NULL;
    }
    return node;
}

/*
 * Initialize a client-supplied node 
 */

hnode_t *kl_hnode_init(hnode_t *hnode, void *data)
{
    hnode->data = data;
    hnode->next = NULL;
    return hnode;
}

/*
 * Destroy a dynamically allocated node.
 */

void kl_hnode_destroy(hnode_t *hnode)
{
    free(hnode);
}

#undef kl_hnode_put
void kl_hnode_put(hnode_t *node, void *data)
{
    node->data = data;
}

#undef kl_hnode_get
void *kl_hnode_get(hnode_t *node)
{
    return node->data;
}

#undef kl_hnode_getkey
const void *kl_hnode_getkey(hnode_t *node)
{
    return node->key;
}

#undef kl_hash_count
hashcount_t kl_hash_count(hash_t *hash)
{
    return hash->nodecount;
}

#undef kl_hash_size
hashcount_t kl_hash_size(hash_t *hash)
{
    return hash->nchains;
}

static hash_val_t hash_fun_default(const void *key)
{
    static unsigned long randbox[] = {
	0x49848f1bU, 0xe6255dbaU, 0x36da5bdcU, 0x47bf94e9U,
	0x8cbcce22U, 0x559fc06aU, 0xd268f536U, 0xe10af79aU,
	0xc1af4d69U, 0x1d2917b5U, 0xec4c304dU, 0x9ee5016cU,
	0x69232f74U, 0xfead7bb3U, 0xe9089ab6U, 0xf012f6aeU,
    };

    const unsigned char *str = key;
    hash_val_t acc = 0;

    while (*str) {
	acc ^= randbox[(*str + acc) & 0xf];
	acc = (acc << 1) | (acc >> 31);
	acc &= 0xffffffffU;
	acc ^= randbox[((*str++ >> 4) + acc) & 0xf];
	acc = (acc << 2) | (acc >> 30);
	acc &= 0xffffffffU;
    }
    return acc;
}

static int hash_comp_default(const void *key1, const void *key2)
{
    return strcmp(key1, key2);
}
