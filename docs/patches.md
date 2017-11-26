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
--- mustache.old        2017-11-26 13:56:27.432368300 +0000
+++ mustache.js 2017-11-23 13:39:14.855455600 +0000
@@ -425,10 +425,11 @@
     }

     if (isFunction(value))
       value = value.call(this.view);

+    if (typeof value === 'undefined') console.warn('  miss',name);
     return value;
   };

   /**
    * A Writer knows how to take a stream of tokens and render them to a
```
