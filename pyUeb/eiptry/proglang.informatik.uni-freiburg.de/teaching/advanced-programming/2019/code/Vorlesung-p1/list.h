struct list_entry;

struct list
{
	struct list_entry *start;
	struct list_entry *end;
};

struct list_entry
{
	struct list_entry *prev;
	struct list_entry *next;
	int val;
};

int *list_insert(struct list *l, struct list_entry *prev, int val);

