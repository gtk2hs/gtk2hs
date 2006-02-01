#include "Gtk2HsStore.h"

#define DEBUG

#ifdef DEBUG
#include <stdio.h>
#define WHEN_DEBUG(a) a
#else
#define WHEN_DEBUG(a)
#endif

static void         gtk2hs_store_init            (Gtk2HsStore      *pkg_tree);
static void         gtk2hs_store_class_init      (Gtk2HsStoreClass *klass);
static void         gtk2hs_store_tree_model_init (GtkTreeModelIface *iface);
static void         gtk2hs_store_finalize        (GObject           *object);
static GtkTreeModelFlags gtk2hs_store_get_flags  (GtkTreeModel      *tree_model);
static gint         gtk2hs_store_get_n_columns   (GtkTreeModel      *tree_model);
static GType        gtk2hs_store_get_column_type (GtkTreeModel      *tree_model,
                                                  gint               index);
static gboolean     gtk2hs_store_get_iter        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreePath       *path);
static GtkTreePath *gtk2hs_store_get_path        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static void         gtk2hs_store_get_value       (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  gint               column,
                                                  GValue            *value);
static gboolean     gtk2hs_store_iter_next       (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gboolean     gtk2hs_store_iter_children   (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *parent);
static gboolean     gtk2hs_store_iter_has_child  (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gint         gtk2hs_store_iter_n_children (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static gboolean     gtk2hs_store_iter_nth_child  (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *parent,
                                                  gint               n);
static gboolean     gtk2hs_store_iter_parent     (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter,
                                                  GtkTreeIter       *child);
static void         gtk2hs_store_ref_node        (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);
static void         gtk2hs_store_unref_node      (GtkTreeModel      *tree_model,
                                                  GtkTreeIter       *iter);

static GObjectClass *parent_class = NULL;


/**
 *
 *  gtk2hs_store_get_type: here we register our new type and its interfaces
 *                         with the type system. If you want to implement
 *                         additional interfaces like GtkTreeSortable, you
 *                         will need to do it here.
 *
 **/
GType
gtk2hs_store_get_type (void)
{
  static GType gtk2hs_store_type = 0;

  if (!gtk2hs_store_type)
  {
    static const GTypeInfo gtk2hs_store_info =
    {
      sizeof (Gtk2HsStoreClass),
      NULL,              /* base_init */
      NULL,              /* base_finalize */
      (GClassInitFunc) gtk2hs_store_class_init,
      NULL,              /* class finalize */
      NULL,              /* class_data */
      sizeof (Gtk2HsStore),
      0,                 /* n_preallocs */
      (GInstanceInitFunc) gtk2hs_store_init
    };

    static const GInterfaceInfo tree_model_info =
    {
      (GInterfaceInitFunc) gtk2hs_store_tree_model_init,
      NULL,
      NULL
    };

    gtk2hs_store_type = g_type_register_static (G_TYPE_OBJECT, "Gtk2HsStore",
                                               &gtk2hs_store_info,
                                               (GTypeFlags) 0);

    g_type_add_interface_static (gtk2hs_store_type,
                                 GTK_TYPE_TREE_MODEL,
                                 &tree_model_info);
  }

  return gtk2hs_store_type;
}


/**
 *
 *  gtk2hs_store_class_init: more boilerplate GObject/GType stuff.
 *                           Init callback for the type system,
 *                           called once when our new class is created.
 *
 **/
static void
gtk2hs_store_class_init (Gtk2HsStoreClass *class)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_class_init\t\t(%p)\n", class));
  GObjectClass *object_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = (GObjectClass*) class;

  object_class->finalize = gtk2hs_store_finalize;
}

/**
 *
 *  gtk2hs_store_tree_model_init: init callback for the interface registration
 *                                in gtk2hs_store_get_type. Here we override
 *                                the GtkTreeModel interface functions that
 *                                we implement.
 *
 **/
static void
gtk2hs_store_tree_model_init (GtkTreeModelIface *iface)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_tree_model_init\t(%p)\n", iface));
  iface->get_flags       = gtk2hs_store_get_flags;
  iface->get_n_columns   = gtk2hs_store_get_n_columns;
  iface->get_column_type = gtk2hs_store_get_column_type;
  iface->get_iter        = gtk2hs_store_get_iter;
  iface->get_path        = gtk2hs_store_get_path;
  iface->get_value       = gtk2hs_store_get_value;
  iface->iter_next       = gtk2hs_store_iter_next;
  iface->iter_children   = gtk2hs_store_iter_children;
  iface->iter_has_child  = gtk2hs_store_iter_has_child;
  iface->iter_n_children = gtk2hs_store_iter_n_children;
  iface->iter_nth_child  = gtk2hs_store_iter_nth_child;
  iface->iter_parent     = gtk2hs_store_iter_parent;
  iface->ref_node        = gtk2hs_store_ref_node;
  iface->unref_node      = gtk2hs_store_unref_node;
}


/**
 *
 *  gtk2hs_store_init: this is called everytime a new custom list object
 *                     instance is created (we do that in gtk2hs_store_new).
 *                     Initialise the list structure's fields here.
 *
 **/
static void
gtk2hs_store_init (Gtk2HsStore *store)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_init\t\t(%p)\n", store));

  store->stamp = g_random_int();  /* Random int to check whether an iter belongs to our model */
}


/**
 *
 *  gtk2hs_store_finalize: this is called just before a custom list is
 *                         destroyed. Free dynamically allocated memory here.
 *
 **/
static void
gtk2hs_store_finalize (GObject *object)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_finalize\t(%p)\n", object));
  Gtk2HsStore *store = GTK2HS_STORE(object);

  /* free all memory used by the store */
  hs_free_stable_ptr(store->impl);

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}


/**
 *
 *  gtk2hs_store_get_flags: tells the rest of the world whether our tree model
 *                          has any special characteristics. In our case,
 *                          we have a list model (instead of a tree), and each
 *                          tree iter is valid as long as the row in question
 *                          exists, as it only contains a pointer to our struct.
 *
 **/
static GtkTreeModelFlags
gtk2hs_store_get_flags (GtkTreeModel *tree_model)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_flags\t\t(%p)\n", tree_model));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  GtkTreeModelFlags result = gtk2hs_store_get_flags_impl(store->impl);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_flags\t\t=%#x\n", result));
  return result;
}


/**
 *
 *  gtk2hs_store_get_n_columns: tells the rest of the world how many data
 *                              columns we export via the tree model interface
 *
 **/
static gint
gtk2hs_store_get_n_columns (GtkTreeModel *tree_model)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_n_columns\t(%p)\n", tree_model));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  return 0;
  /*
  gint result = gtk2hs_store_get_n_columns_impl(store->impl);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_n_columns\t=%d\n", result));
  return result;
  */
}


/**
 *
 *  gtk2hs_store_get_column_type: tells the rest of the world which type of
 *                                data an exported model column contains
 *
 **/
static GType
gtk2hs_store_get_column_type (GtkTreeModel *tree_model,
                              gint          index)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_column_type\t(%p, %d)\n", tree_model, index));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));
  
  return G_TYPE_INVALID;
  /*
  GType result = gtk2hs_store_get_column_type_impl(store->impl, index);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_column_type\t=%s\n", g_type_name(result)));
  return result;
  */
}


/**
 *
 *  gtk2hs_store_get_iter: converts a tree path (physical position) into a
 *                         tree iter structure (the content of the iter
 *                         fields will only be used internally by our model).
 *
 **/
static gboolean
gtk2hs_store_get_iter (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter,
                       GtkTreePath  *path)
{
  gchar *path_str = gtk_tree_path_to_string(path);
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_iter\t\t(%p, %p, \"%s\")\n", tree_model, iter, path_str));
  g_free(path_str);
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  /* We simply store a pointer to our custom record in the iter */
  iter->stamp     = store->stamp;
  gboolean result = gtk2hs_store_get_iter_impl(store->impl, iter, path);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_iter\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_get_path: converts a tree iter into a tree path (ie. the
 *                         physical position of that row in the list).
 *
 **/
static GtkTreePath *
gtk2hs_store_get_path (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_path\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));
  
  GtkTreePath * result = gtk2hs_store_get_path_impl(store->impl, iter);
  gchar *result_str = gtk_tree_path_to_string(result);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_path\t\t=\"%s\"\n", result_str));
  g_free(result_str);
  return result;
}


/**
 *
 *  gtk2hs_store_get_value: Returns a row's exported data columns
 *                          (_get_value is what gtk_tree_model_get uses)
 *
 **/
static void
gtk2hs_store_get_value (GtkTreeModel *tree_model,
                        GtkTreeIter  *iter,
                        gint          column,
                        GValue       *value)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_get_value\t\t(%p, %p, %d, %p)\n", tree_model, iter, column, value));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  g_value_init(value, G_TYPE_INVALID);
  
  /*
    gtk2hs_store_get_value_impl(store->impl, iter, column, value);
    gchar *result = g_strdup_value_contents(value);
    WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_get_value\t\t=%s\n", result));
    g_free(result);
  */
}


/**
 *
 *  gtk2hs_store_iter_next: Takes an iter structure and sets it to point
 *                          to the next row.
 *
 **/
static gboolean
gtk2hs_store_iter_next (GtkTreeModel  *tree_model,
                        GtkTreeIter   *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_next\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gboolean result = gtk2hs_store_iter_next_impl(store->impl, iter);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_next\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_children: Returns TRUE or FALSE depending on whether
 *                              the row specified by 'parent' has any children.
 *                              If it has children, then 'iter' is set to
 *                              point to the first child. Special case: if
 *                              'parent' is NULL, then the first top-level
 *                              row should be returned if it exists.
 *
 **/
static gboolean
gtk2hs_store_iter_children (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter,
                            GtkTreeIter  *parent)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_children\t(%p, %p, %p)\n", tree_model, iter, parent));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gboolean result = gtk2hs_store_iter_children_impl(store->impl, iter, parent);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_children\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_has_child: Returns TRUE or FALSE depending on whether
 *                               the row specified by 'iter' has any children.
 *
 **/
static gboolean
gtk2hs_store_iter_has_child (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_has_child\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));
  
  gboolean result = gtk2hs_store_iter_has_child_impl(store->impl, iter);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_has_child\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_n_children: Returns the number of children the row
 *                                specified by 'iter' has. This is usually 0,
 *                                as we only have a list and thus do not have
 *                                any children to any rows. A special case is
 *                                when 'iter' is NULL, in which case we need
 *                                to return the number of top-level nodes,
 *                                ie. the number of rows in our list.
 *
 **/
static gint
gtk2hs_store_iter_n_children (GtkTreeModel *tree_model,
                              GtkTreeIter  *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_n_children\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gboolean result = gtk2hs_store_iter_n_children_impl(store->impl, iter);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_n_children\t=%d\n", result));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_nth_child: If the row specified by 'parent' has any
 *                               children, set 'iter' to the n-th child and
 *                               return TRUE if it exists, otherwise FALSE.
 *                               A special case is when 'parent' is NULL, in
 *                               which case we need to set 'iter' to the n-th
 *                               row if it exists.
 *
 **/
static gboolean
gtk2hs_store_iter_nth_child (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter,
                             GtkTreeIter  *parent,
                             gint          n)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_nth_child\t(%p, %p, %p, %d)\n", tree_model, iter, parent, n));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gboolean result = gtk2hs_store_iter_nth_child_impl(store->impl, iter, parent, n);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_nth_child\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


/**
 *
 *  gtk2hs_store_iter_parent: Point 'iter' to the parent node of 'child'. As
 *                            we have a list and thus no children and no
 *                            parents of children, we can just return FALSE.
 *
 **/
static gboolean
gtk2hs_store_iter_parent (GtkTreeModel *tree_model,
                          GtkTreeIter  *iter,
                          GtkTreeIter  *child)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_iter_parent\t\t(%p, %p, %p)\n", tree_model, iter, child));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));
 
  gboolean result = gtk2hs_store_iter_parent_impl(store->impl, iter, child);
  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_iter_parent\t\t=%s\n", result ? "TRUE" : "FALSE"));
  return result;
}


static void
gtk2hs_store_ref_node (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_ref_node\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gtk2hs_store_ref_node_impl(store->impl, iter);
}

static void
gtk2hs_store_unref_node (GtkTreeModel *tree_model,
                         GtkTreeIter  *iter)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_unref_node\t\t(%p, %p)\n", tree_model, iter));
  Gtk2HsStore *store = (Gtk2HsStore *) tree_model;
  g_assert (GTK2HS_IS_STORE(tree_model));

  gtk2hs_store_unref_node_impl(store->impl, iter);
}

/**
 *
 *  gtk2hs_store_new:  This is what you use in your own code to create a
 *                     new custom list tree model for you to use.
 *
 **/
Gtk2HsStore *
gtk2hs_store_new (HsStablePtr impl)
{
  WHEN_DEBUG(fprintf(stderr, "calling gtk2hs_store_new\t\t(%p)\n", impl));
  Gtk2HsStore *newstore = (Gtk2HsStore*) g_object_new (GTK2HS_TYPE_STORE, NULL);

  newstore->impl = impl;

  WHEN_DEBUG(fprintf(stderr, "return  gtk2hs_store_new\t\t=%p\n", newstore));
  return newstore;
}
