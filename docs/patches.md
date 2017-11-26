# Patches

## Patch hogan.js for debugging

```diff
--- template.old    2017-11-26 10:16:35.342614962 +0000
+++ template.js 2017-11-26 10:14:38.355424901 +0000
@@ -183,10 +183,11 @@
           break;
         }
       }
 
       if (!found) {
+        console.warn('  miss: '+key);
         return (returnFound) ? false : "";
       }
 
       if (!returnFound && typeof val == 'function') {
         val = this.mv(val, ctx, partials);
```

## Patch mustache.js for debugging

```diff
```
