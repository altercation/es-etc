import offlineimap
import re

#prioritized = ['INBOX', '[Gmail]', 'bulk', 'notifications', 'social-updates', 'lists']
prioritized = ['bulk', 'notifications', 'social-updates', 'lists', 'INBOX', '[Gmail]']

def mycmp(x, y):
  for prefix in prioritized:
    if offlineimap.__version__ < '6.4':
      xsw = x.startswith(prefix)
      ysw = y.startswith(prefix)
    else:
      xsw = x.visiblename.startswith(prefix)
      ysw = y.visiblename.startswith(prefix)
    if xsw and ysw:
      return cmp(x, y)
    elif xsw:
      return -1
    elif ysw:
      return +1
  return cmp(x, y)

