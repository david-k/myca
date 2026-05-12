# In gdb, simply run `source dbg_pretty_printer.py` to register the pretty printers

import gdb
import gdb.printing

class TypePrinter(gdb.ValuePrinter):
    """Print a Type"""

    def __init__(self, val):
        self.__val = val

    def to_string(self):
        # It's important to use int(self.__val.address) inside the format
        # string and not self.__val.address as that would trigger the
        # pretty printing logic. (self.__val.address has type Type*, and
        # since we also handle pretty printing for Type*, this would lead
        # to an infinite recursion)
        return gdb.parse_and_eval(f"dbg_print_type(*((Type*){int(self.__val.address)}))")


class FixedArrayPrinter(gdb.ValuePrinter):
    """Print a FixedArray<T>"""

    def __init__(self, val):
        self.__val = val

    def to_string(self):
        element_type = self.__val.type.template_argument(0)
        count = self.__val["count"]
        return f"{element_type.name}[{count}]"

    def children(self):
        def element_iterator():
            element_type = self.__val.type.template_argument(0)
            count = int(self.__val["count"])
            cur_element = self.__val["items"].address.cast(element_type.pointer())
            for i in range(count):
                yield (str(i), cur_element.dereference())
                cur_element = cur_element + 1

        return element_iterator()

    def display_hint(self):
        return 'array'


def try_get_class_name(typ):
    typ = typ.strip_typedefs()
    if not typ.name:
        return None

    return typ.name.split("<")[0] # Strip potential template arguments

def lookup_pretty_printer(value):
    if value.type.code == gdb.TYPE_CODE_PTR:
        value = value.dereference()

    name = try_get_class_name(value.type)
    if name == "Type":
        return TypePrinter(value)
    if name == "FixedArray":
        return FixedArrayPrinter(value)


gdb.printing.register_pretty_printer(gdb.current_objfile(), lookup_pretty_printer)


# Registering a pretty printer for a type can also be done more simply using
# regexes:
#
#   def build_pretty_printer():
#       pp = gdb.printing.RegexpCollectionPrettyPrinter("myca")
#       pp.add_printer('Type', '^Type$', TypePrinter)
#       return pp
#
#   gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer())
#
# However, pretty printers registered this way won't be used when displaying
# local pointer variables via DAP, even when derefencing them
