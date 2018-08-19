from datetime import datetime, timedelta, timezone
from pytz import utc
from typing import Optional

from imf._imf import ffi, lib  # pylint: disable=E0611

epoch = datetime(1970, 1, 1, tzinfo=utc)


def cstring(cstr: ffi.CData) -> str:
    try:
        return ffi.string(cstr).decode("utf8")
    finally:
        lib.hs_free_cstring(cstr)


def init():
    lib.imf_init()


def exit():
    lib.imf_exit()


def not_implemented(*args, **kwargs):
    raise NotImplementedError


class HsStablePtr:

    parse_fn = not_implemented
    format_fn = not_implemented

    def __init__(self, ptr: ffi.CData) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        lib.hs_free_stable_ptr(self._ptr)

    def __repr__(self) -> str:
        return "%s(%r)" % (self.__class__.__name__, self._ptr)

    def __str__(self) -> str:
        return cstring(self.format_fn(self._ptr))

    @classmethod
    def parse(cls, raw: str) -> "HsStablePtr":
        ptr = cls.parse_fn(ffi.new("char[]", raw.encode("utf8")))
        if ptr:
            return cls(ptr)
        else:
            raise ValueError("failed to parse %s %s" % (cls.__name__, raw))


class HsStableItem(HsStablePtr):

    equal_fn = not_implemented

    def __eq__(self, other: "HsStableItem") -> bool:
        return self.equal_fn(self._ptr, other._ptr)


class HsStableList(HsStablePtr):

    item_class = not_implemented

    length_fn = not_implemented
    item_fn = not_implemented

    def __len__(self) -> int:
        return self.length_fn(self._ptr)

    def __getitem__(self, key: int) -> "HsStableItem":
        return self.item_class(self.item_fn(self._ptr, key))

    def __iter__(self):
        for i in range(0, len(self)):
            yield self[i]


class Mailbox(HsStableItem):

    parse_fn = lib.mailbox_parse
    format_fn = lib.mailbox_format
    equal_fn = lib.mailbox_equals

    @classmethod
    def from_parts(cls, display: Optional[str], local: str, domain: str) -> "Mailbox":
        ptr = lib.mailbox_from_parts(
            ffi.new("char[]", (display or "").encode("utf8")),
            ffi.new("char[]", local.encode("utf8")),
            ffi.new("char[]", domain.encode("utf8")),
        )
        if ptr:
            return cls(ptr)
        else:
            raise ValueError("failed to build Mailbox %s" % [display, local, domain])

    @property
    def display(self) -> str:
        return cstring(lib.mailbox_display(self._ptr))

    @property
    def local(self) -> str:
        return cstring(lib.mailbox_local(self._ptr))

    @property
    def domain(self) -> str:
        return cstring(lib.mailbox_domain(self._ptr))


class MailboxList(HsStableList):

    item_class = Mailbox

    parse_fn = lib.mailbox_list_parse
    format_fn = lib.mailbox_list_format
    length_fn = lib.mailbox_list_length
    item_fn = lib.mailbox_list_item


class DateTime(HsStableItem):

    parse_fn = lib.datetime_parse
    format_fn = lib.datetime_format
    equal_fn = lib.datetime_equals

    @classmethod
    def from_datetime(cls, t: datetime) -> "DateTime":
        t = t.astimezone(t.tzinfo)  # make sure tzinfo is set
        ns = int((t - epoch).total_seconds() * 1e9)
        tzoffset = int(t.tzinfo.utcoffset(t).total_seconds() / 60)
        ptr = lib.datetime_from_nanoseconds(ns, tzoffset)
        if ptr:
            return cls(ptr)
        else:
            raise ValueError("failed to build Datetime %s" % t)

    @property
    def seconds(self) -> float:
        return lib.datetime_nanoseconds(self._ptr) / 1e9

    @property
    def tzoffset(self) -> int:
        return lib.datetime_tzoffset(self._ptr)

    def to_datetime(self) -> datetime:
        tz = timezone(timedelta(minutes=self.tzoffset))
        return datetime.fromtimestamp(self.seconds, utc).astimezone(tz)


class MessageId(HsStableItem):

    parse_fn = lib.message_id_parse
    format_fn = lib.message_id_format
    equal_fn = lib.message_id_equals

    @classmethod
    def from_parts(cls, left: str, right: str) -> "MessageId":
        ptr = lib.message_id_from_parts(
            ffi.new("char[]", left.encode("utf8")),
            ffi.new("char[]", right.encode("utf8")),
        )
        if ptr:
            return cls(ptr)
        else:
            raise ValueError("failed to build MessageId %s" % [left, right])

    @property
    def left(self) -> str:
        return cstring(lib.message_id_left(self._ptr))

    @property
    def right(self) -> str:
        return cstring(lib.message_id_right(self._ptr))


class HeaderField(HsStableItem):

    parse_fn = lib.header_field_parse
    format_fn = lib.header_field_format
    equal_fn = lib.header_field_equals

    @property
    def name(self) -> str:
        return cstring(lib.header_field_name(self._ptr))

    @property
    def value(self) -> str:
        return cstring(lib.header_field_value(self._ptr))


class Header(HsStableList):

    item_class = HeaderField

    parse_fn = lib.header_parse
    format_fn = lib.header_format
    length_fn = lib.header_length
    item_fn = lib.header_item


class Message(HsStableItem):

    parse_fn = lib.message_parse
    format_fn = lib.message_format
    equal_fn = lib.message_equals

    @property
    def header(self) -> "Header":
        return Header(lib.message_header(self._ptr))

    @property
    def body(self) -> str:
        return cstring(lib.message_body(self._ptr))
