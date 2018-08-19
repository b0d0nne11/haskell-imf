from datetime import datetime, timedelta, timezone
from pytz import utc
from nose.tools import ok_, eq_, assert_raises

import imf
from imf import Mailbox, MailboxList, DateTime, MessageId, HeaderField, Header, Message


def setup_module():
    imf.init()


def teardown_module():
    imf.exit()


def test_mailbox():
    raw_mailbox = "John <john@example.com>"
    mbox = Mailbox.from_parts("John", "john", "example.com")

    eq_(str(mbox), raw_mailbox)
    eq_(mbox.display, "John")
    eq_(mbox.local, "john")
    eq_(mbox.domain, "example.com")

    with assert_raises(ValueError):
        Mailbox.parse("")


def test_mailbox_list():
    raw_mboxes = "John <john@example.com>, Bob <bob@example.com>"
    mboxes = MailboxList.parse(raw_mboxes)

    eq_(str(mboxes), raw_mboxes)
    eq_(len(mboxes), 2)
    eq_(mboxes[0], Mailbox.parse("john@example.com"))
    ok_(Mailbox.parse("bob@example.com") in mboxes)

    with assert_raises(ValueError):
        MailboxList.parse("")


def test_datetime():
    raw_datetime = "Thu, 13 Sep 1984 00:00:00 -0500"
    obj_datetime = datetime(1984, 9, 13, tzinfo=timezone(timedelta(hours=-5)))
    t = DateTime.parse(raw_datetime)

    eq_(str(t), raw_datetime)
    eq_(t.seconds, 463899600)
    eq_(t.tzoffset, -300)
    eq_(t.to_datetime(), obj_datetime)

    with assert_raises(ValueError):
        DateTime.parse("")


def test_message_id():
    raw_msgid = "<1234@example.com>"
    msgid = MessageId.parse(raw_msgid)

    eq_(str(msgid), raw_msgid)
    eq_(msgid.left, "1234")
    eq_(msgid.right, "example.com")

    with assert_raises(ValueError):
        MessageId.parse("")


def test_header_field():
    raw_field = "From: john@example.com\r\n"
    field = HeaderField.parse(raw_field)

    eq_(str(field), raw_field)
    eq_("From", field.name)
    eq_("john@example.com", field.value)

    with assert_raises(ValueError):
        HeaderField.parse("")


def test_header():
    raw_header = "".join(
        [
            "Date: Thu, 13 Sep 1984 00:00:00 -0500\r\n",
            "Message-Id: <1234@example.com>\r\n",
            "From: john@example.com\r\n",
            "To: Bob <bob@example.com>\r\n",
            "Cc: Carl <carl@example.com>, Zeke <zeke@example.com>\r\n",
            "Bcc: \r\n",
            "Subject: This is a test!\r\n",
        ]
    )
    header = Header.parse(raw_header)

    eq_(str(header), raw_header)
    eq_(len(header), 7)
    eq_(header[0], HeaderField.parse("Date: Thu, 13 Sep 1984 00:00:00 -0500\r\n"))
    ok_(HeaderField.parse("Message-Id: <1234@example.com>\r\n") in header)

    with assert_raises(ValueError):
        Header.parse("")


def test_message():
    raw_message = "".join(
        [
            "From: John Doe <jdoe@machine.example>\r\n",
            "To: Mary Smith <mary@example.net>\r\n",
            "Subject: Saying Hello\r\n",
            "Date: Fri, 21 Nov 1997 09:55:06 -0600\r\n",
            "Message-ID: <1234@local.machine.example>\r\n",
            "\r\n",
            "This is a message just to say hello.\r\n",
            "So, hello.\r\n",
        ]
    )
    message = Message.parse(raw_message)

    eq_(str(message), raw_message)
    eq_(
        [(f.name, f.value) for f in message.header],
        [
            ("From", "John Doe <jdoe@machine.example>"),
            ("To", "Mary Smith <mary@example.net>"),
            ("Subject", "Saying Hello"),
            ("Date", "Fri, 21 Nov 1997 09:55:06 -0600"),
            ("Message-ID", "<1234@local.machine.example>"),
        ],
    )
    eq_(message.body, "This is a message just to say hello.\r\nSo, hello.\r\n")

    with assert_raises(ValueError):
        Message.parse("")
