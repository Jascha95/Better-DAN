#include "list.h"

#include <stdlib.h>

int *list_insert(struct list *l, struct list_entry *prev, int val)
{
	struct list_entry *new_entry = malloc(sizeof(struct list_entry));

	if(!new_entry) {
		return 0;
	}

	new_entry->prev = prev;
	new_entry->val = val;

	if(prev)
	{
		new_entry->next = prev->next;
		prev->next->prev = new_entry;
		prev->next = new_entry;
	}
	else
	{
		new_entry->next = l->start;
		l->start->prev = new_entry;
		l->start = new_entry;
	}

	return &(new_entry->val);
}

