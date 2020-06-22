;; Copyright © 2016-2019 Calvin Rose and contributors
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions: The above copyright
;; notice and this permission notice shall be included in all copies or
;; substantial portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS",
;; WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(include :utils)
(include :parser)
(include :compiler)
(include :specials)

(local utils (require :utils))
(local parser (require :parser))
(local compiler (require :compiler))
(local specials (require :specials))

(fn eval [str options ...]
  (let [opts (utils.copy options)
        _ (when (and (= opts.allowedGlobals nil)
                     (not (getmetatable opts.env)))
            (set opts.allowedGlobals (specials.currentGlobalNames opts.env)))
        env (and opts.env (specials.wrapEnv opts.env))
        lua-source (compiler.compileString str opts)
        loader (specials.loadCode lua-source env (or opts.filename str))]
    (set opts.filename nil)
    (loader ...)))

(fn dofile-fennel [filename options ...]
  (let [opts (utils.copy options)
        f (assert (io.open filename :rb))
        source (f:read :*all)]
    (f:close)
    (set opts.filename filename)
    (eval source opts ...)))

;; there is currently an unfortunate circular reference between this code
;; and the specials module due to loading of macros
(set compiler.dofileFennel dofile-fennel)

(local path-table ["./?.fnl" "./?/init.fnl"])
(table.insert path-table (utils.getenv "FENNEL_PATH"))

(fn make-searcher [options]
  (fn [module-name]
    (let [opts (utils.copy utils.rootOptions)
          filename (specials.searchModule module-name)]
      (each [k v (pairs (or options {}))]
        (tset opts k v))
      (if filename
          (fn [mod-name]
            (dofile-fennel filename opts mod-name))))))

(local m {
          :list utils.list
          :sym utils.sym
          :varg utils.varg

          :parser parser.parser
          :granulate parser.grandulate
          :stringStream parser.stringStream

          :compile compiler.compile
          :compileString compiler.compileString
          :compileStream compiler.compileStream
          :compile1 compiler.compile1
          :traceback compiler.traceback
          :mangle compiler.globalMangling
          :unmangle compiler.globalUnmangling
          :metadata compiler.metadata
          :scope compiler.makeScope
          :gensym compiler.gensym

          :loadCode specials.loadCode
          :doc specials.doc
          :macroLoaded specials.macroLoaded
          :searchModule specials.searchModule

          :eval eval
          :dofile dofile-fennel
          :path (table.concat path-table ";")
          :version "0.5.0-dev"
          :makeSearcher make-searcher
          :make_searcher make-searcher
          :searcher (make-searcher)
          })

(local repl-source "(local (fennel internals) ...)

(fn default-read-chunk [parser-state]
  (io.write (if (< 0 parser-state.stackSize) \"..\" \">> \"))
  (io.flush)
  (let [input (io.read)]
    (and input (.. input \"\n\"))))

(fn default-on-values [xs]
  (io.write (table.concat xs \"\t\"))
  (io.write \"\n\"))

(fn default-on-error [errtype err lua-source]
  (io.write
   (match errtype
     \"Lua Compile\" (.. \"Bad code generated - likely a bug with the compiler:\n\"
                       \"--- Generated Lua Start ---\n\"
                       lua-source
                       \"--- Generated Lua End ---\n\")
     \"Runtime\" (.. (fennel.traceback err 4) \"\n\")
     _ (: \"%s error: %s\n\" :format errtype (tostring err)))))

(local save-source
       (table.concat [\"local ___i___ = 1\"
                      \"while true do\"
                      \" local name, value = debug.getlocal(1, ___i___)\"
                      \" if(name and name ~= \\\"___i___\\\") then\"
                      \" ___replLocals___[name] = value\"
                      \" ___i___ = ___i___ + 1\"
                      \" else break end end\"] \"\n\"))

(fn splice-save-locals [env lua-source]
  (set env.___replLocals___ (or env.___replLocals___ {}))
  (let [spliced-source []
        bind \"local %s = ___replLocals___['%s']\"]
    (each [line (lua-source:gmatch \"([^\n]+)\n?\")]
      (table.insert spliced-source line))
    (each [name (pairs env.___replLocals___)]
      (table.insert spliced-source 1 (bind:format name name)))
    (when (and (< 1 (# spliced-source))
               (: (. spliced-source (# spliced-source)) :match \"^ *return .*$\"))
      (table.insert spliced-source (# spliced-source) save-source))
    (table.concat spliced-source \"\n\")))

(fn completer [env scope text]
  (let [matches []
        input-fragment (text:gsub \".*[%s)(]+\" \"\")]
    (fn add-partials [input tbl prefix] ; add partial key matches in tbl
      (each [k (internals.allPairs tbl)]
        (let [k (if (or (= tbl env) (= tbl env.___replLocals___))
                    (. scope.unmanglings k)
                    k)]
          (when (and (< (# matches) 2000) ; stop explosion on too many items
                     (= (type k) \"string\")
                     (= input (k:sub 0 (# input))))
            (table.insert matches (.. prefix k))))))
    (fn add-matches [input tbl prefix] ; add matches, descending into tbl fields
      (let [prefix (if prefix (.. prefix \".\") \"\")]
        (if (not (input:find \"%.\")) ; no more dots, so add matches
            (add-partials input tbl prefix)
            (let [(head tail) (input:match \"^([^.]+)%.(.*)\")
                  raw-head (if (or (= tbl env) (= tbl env.___replLocals___))
                               (. scope.manglings head)
                               head)]
              (when (= (type (. tbl raw-head)) \"table\")
                (add-matches tail (. tbl raw-head) (.. prefix head)))))))

    (add-matches input-fragment (or scope.specials []))
    (add-matches input-fragment (or scope.macros []))
    (add-matches input-fragment (or env.___replLocals___ []))
    (add-matches input-fragment env)
    (add-matches input-fragment (or env._ENV env._G []))
    matches))

(fn repl [options]
  (let [old-root-options internals.rootOptions
        env (if options.env
                (internals.wrapEnv options.env)
                (setmetatable {} {:__index (or _G._ENV _G)}))
        save-locals? (and (not= options.saveLocals false)
                          env.debug env.debug.getlocal)
        opts {}
        _ (each [k v (pairs options)] (tset opts k v))
        read-chunk (or opts.readChunk default-read-chunk)
        on-values (or opts.onValues default-on-values)
        on-error (or opts.onError default-on-error)
        pp (or opts.pp tostring)
        ;; make parser
        (byte-stream clear-stream) (fennel.granulate read-chunk)
        chars []
        (read reset) (fennel.parser (fn [parser-state]
                                      (let [c (byte-stream parser-state)]
                                        (tset chars (+ (# chars) 1) c)
                                        c)))
        scope (fennel.scope)]

    ;; use metadata unless we've specifically disabled it
    (set opts.useMetadata (not= options.useMetadata false))
    (when (= opts.allowedGlobals nil)
      (set opts.allowedGlobals (internals.currentGlobalNames opts.env)))

    (when opts.registerCompleter
      (opts.registerCompleter (partial completer env scope)))

    (fn loop []
      (each [k (pairs chars)] (tset chars k nil))
      (let [(ok parse-ok? x) (pcall read)
            src-string (string.char ((or _G.unpack table.unpack) chars))]
        (internals.setRootOptions opts)
        (if (not ok)
            (do (on-error \"Parse\" parse-ok?)
                (clear-stream)
                (reset)
                (loop))
            (when parse-ok? ; if this is false, we got eof
              (match (pcall fennel.compile x {:correlate opts.correlate
                                              :source src-string
                                              :scope scope
                                              :useMetadata opts.useMetadata
                                              :moduleName opts.moduleName
                                              :assert-compile opts.assert-compile
                                              :parse-error opts.parse-error})
                (false msg) (do (clear-stream)
                                (on-error \"Compile\" msg))
                (true source) (let [source (if save-locals?
                                               (splice-save-locals env source)
                                               source)
                                    (lua-ok? loader) (pcall fennel.loadCode
                                                            source env)]
                                (if (not lua-ok?)
                                    (do (clear-stream)
                                        (on-error \"Lua Compile\" loader source))
                                    (match (xpcall #[(loader)]
                                                   (partial on-error \"Runtime\"))
                                      (true ret)
                                      (do (set env._ (. ret 1))
                                          (set env.__ ret)
                                          (on-values (internals.map ret pp)))))))
              (internals.setRootOptions old-root-options)
              (loop)))))
    (loop)))")

(fn m.repl [options]
  ((eval repl-source {} m {:rootOptions utils.rootOptions
                           :setRootOptions (fn [r] (set utils.rootOptions r))
                           :currentGlobalNames specials.currentGlobalNames
                           :wrapEnv utils.wrapEnv
                           :allPairs utils.allPairs
                           :map utils.map}) options))

(local builtin-macros "
{\"->\" (fn [val ...]
        \"Thread-first macro.
Take the first value and splice it into the second form as its first argument.
The value of the second form is spliced into the first arg of the third, etc.\"
        (var x val)
        (each [_ e (ipairs [...])]
          (let [elt (if (list? e) e (list e))]
            (table.insert elt 2 x)
            (set x elt)))
        x)
 \"->>\" (fn [val ...]
         \"Thread-last macro.
Same as ->, except splices the value into the last position of each form
rather than the first.\"
         (var x val)
         (each [_ e (pairs [...])]
           (let [elt (if (list? e) e (list e))]
             (table.insert elt x)
             (set x elt)))
         x)
 \"-?>\" (fn [val ...]
         \"Nil-safe thread-first macro.
Same as -> except will short-circuit with nil when it encounters a nil value.\"
         (if (= 0 (select \"#\" ...))
             val
             (let [els [...]
                   e (table.remove els 1)
                   el (if (list? e) e (list e))
                   tmp (gensym)]
               (table.insert el 2 tmp)
               `(let [,tmp ,val]
                  (if ,tmp
                      (-?> ,el ,(unpack els))
                      ,tmp)))))
 \"-?>>\" (fn [val ...]
         \"Nil-safe thread-last macro.
Same as ->> except will short-circuit with nil when it encounters a nil value.\"
          (if (= 0 (select \"#\" ...))
              val
              (let [els [...]
                    e (table.remove els 1)
                    el (if (list? e) e (list e))
                    tmp (gensym)]
                (table.insert el tmp)
                `(let [,tmp ,val]
                   (if ,tmp
                       (-?>> ,el ,(unpack els))
                       ,tmp)))))
 :doto (fn [val ...]
         \"Evaluates val and splices it into the first argument of subsequent forms.\"
         (let [name (gensym)
               form `(let [,name ,val])]
           (each [_ elt (pairs [...])]
             (table.insert elt 2 name)
             (table.insert form elt))
           (table.insert form name)
           form))
 :when (fn [condition body1 ...]
         \"Evaluate body for side-effects only when condition is truthy.\"
         (assert body1 \"expected body\")
         `(if ,condition
              (do ,body1 ,...)))
 :with-open (fn [closable-bindings ...]
              \"Like `let`, but invokes (v:close) on every binding after evaluating the body.
The body is evaluated inside `xpcall` so that bound values will be closed upon
encountering an error before propagating it.\"
              (let [bodyfn    `(fn [] ,...)
                    closer    `(fn close-handlers# [ok# ...] (if ok# ... (error ... 0)))
                    traceback `(. (or package.loaded.fennel debug) :traceback)]
                (for [i 1 (# closable-bindings) 2]
                  (assert (sym? (. closable-bindings i))
                    \"with-open only allows symbols in bindings\")
                  (table.insert closer 4 `(: ,(. closable-bindings i) :close)))
                `(let ,closable-bindings ,closer
                   (close-handlers# (xpcall ,bodyfn ,traceback)))))
 :partial (fn [f ...]
            \"Returns a function with all arguments partially applied to f.\"
            (let [body (list f ...)]
              (table.insert body _VARARG)
              `(fn [,_VARARG] ,body)))
 :pick-args (fn [n f]
               \"Creates a function of arity n that applies its arguments to f.
For example,\n\t(pick-args 2 func)
expands to\n\t(fn [_0_ _1_] (func _0_ _1_))\"
               (assert (and (= (type n) :number) (= n (math.floor n)) (>= n 0))
                 \"Expected n to be an integer literal >= 0.\")
               (let [bindings []]
                 (for [i 1 n] (tset bindings i (gensym)))
                 `(fn ,bindings (,f ,(unpack bindings)))))
 :pick-values (fn [n ...]
                 \"Like the `values` special, but emits exactly n values.\nFor example,
\t(pick-values 2 ...)\nexpands to\n\t(let [(_0_ _1_) ...] (values _0_ _1_))\"
                 (assert (and (= :number (type n)) (>= n 0) (= n (math.floor n)))
                         \"Expected n to be an integer >= 0\")
                 (let [let-syms   (list)
                       let-values (if (= 1 (select :# ...)) ... `(values ,...))]
                   (for [i 1 n] (table.insert let-syms (gensym)))
                   (if (= n 0) `(values)
                       `(let [,let-syms ,let-values] (values ,(unpack let-syms))))))
 :lambda (fn [...]
           \"Function literal with arity checking.
Will throw an exception if a declared argument is passed in as nil, unless
that argument name begins with ?.\"
           (let [args [...]
                 has-internal-name? (sym? (. args 1))
                 arglist (if has-internal-name? (. args 2) (. args 1))
                 docstring-position (if has-internal-name? 3 2)
                 has-docstring? (and (> (# args) docstring-position)
                                     (= :string (type (. args docstring-position))))
                 arity-check-position (- 4 (if has-internal-name? 0 1) (if has-docstring? 0 1))]
             (fn check! [a]
               (if (table? a)
                   (each [_ a (pairs a)]
                     (check! a))
                   (and (not (: (tostring a) :match \"^?\"))
                        (not= (tostring a) \"&\")
                        (not= (tostring a) \"...\"))
                   (table.insert args arity-check-position
                                 `(assert (not= nil ,a)
                                          (: \"Missing argument %s on %s:%s\"
                                             :format ,(tostring a)
                                             ,(or a.filename \"unknown\")
                                             ,(or a.line \"?\"))))))
             (assert (> (length args) 1) \"expected body expression\")
             (each [_ a (ipairs arglist)]
               (check! a))
             `(fn ,(unpack args))))
 :macro (fn macro [name ...]
          \"Define a single macro.\"
          (assert (sym? name) \"expected symbol for macro name\")
          (local args [...])
          `(macros { ,(tostring name) (fn ,name ,(unpack args))}))
 :macrodebug (fn macrodebug [form return?]
              \"Print the resulting form after performing macroexpansion.
With a second argument, returns expanded form as a string instead of printing.\"
              (let [(ok view) (pcall require :fennelview)
                    handle (if return? `do `print)]
                `(,handle ,((if ok view tostring) (macroexpand form _SCOPE)))))
 :import-macros (fn import-macros [binding1 module-name1 ...]
                  \"Binds a table of macros from each macro module according to its binding form.
Each binding form can be either a symbol or a k/v destructuring table.
Example:\n  (import-macros mymacros                 :my-macros    ; bind to symbol
                 {:macro1 alias : macro2} :proj.macros) ; import by name\"
                  (assert (and binding1 module-name1 (= 0 (% (select :# ...) 2)))
                          \"expected even number of binding/modulename pairs\")
                  (for [i 1 (select :# binding1 module-name1 ...) 2]
                    (local (binding modname) (select i binding1 module-name1 ...))
                    ;; generate a subscope of current scope, use require-macros to bring in macro
                    ;; module. after that, we just copy the macros from subscope to scope.
                    (local scope (get-scope))
                    (local subscope (fennel.scope scope))
                    (fennel.compileString (string.format \"(require-macros %q)\" modname)
                                          {:scope subscope})
                    (if (sym? binding)
                      ;; bind whole table of macros to table bound to symbol
                      (do (tset scope.macros (. binding 1) {})
                          (each [k v (pairs subscope.macros)]
                            (tset (. scope.macros (. binding 1)) k v)))

                      ;; 1-level table destructuring for importing individual macros
                      (table? binding)
                      (each [macro-name [import-key] (pairs binding)]
                        (assert (= :function (type (. subscope.macros macro-name)))
                                (.. \"macro \" macro-name \" not found in module \" modname))
                        (tset scope.macros import-key (. subscope.macros macro-name)))))
                  ;; TODO: replace with `nil` once we fix macros being able to return nil
                  `(do nil))
 :match
(fn match [val ...]
  \"Perform pattern matching on val. See reference for details.\"
  ;; this function takes the AST of values and a single pattern and returns a
  ;; condition to determine if it matches as well as a list of bindings to
  ;; introduce for the duration of the body if it does match.
  (fn match-pattern [vals pattern unifications]
    ;; we have to assume we're matching against multiple values here until we
    ;; know we're either in a multi-valued clause (in which case we know the #
    ;; of vals) or we're not, in which case we only care about the first one.
    (let [[val] vals]
      (if (or (and (sym? pattern) ; unification with outer locals (or nil)
                   (not= :_ (tostring pattern)) ; never unify _
                   (or (in-scope? pattern)
                       (= :nil (tostring pattern))))
              (and (multi-sym? pattern)
                   (in-scope? (. (multi-sym? pattern) 1))))
          (values `(= ,val ,pattern) [])
          ;; unify a local we've seen already
          (and (sym? pattern)
               (. unifications (tostring pattern)))
          (values `(= ,(. unifications (tostring pattern)) ,val) [])
          ;; bind a fresh local
          (sym? pattern)
          (let [wildcard? (= (tostring pattern) \"_\")]
            (if (not wildcard?) (tset unifications (tostring pattern) val))
            (values (if (or wildcard? (: (tostring pattern) :find \"^?\"))
                        true `(not= ,(sym :nil) ,val))
                    [pattern val]))
          ;; guard clause
          (and (list? pattern) (sym? (. pattern 2)) (= :? (tostring (. pattern 2))))
          (let [(pcondition bindings) (match-pattern vals (. pattern 1)
                                                     unifications)
                condition `(and ,pcondition)]
            (for [i 3 (# pattern)] ; splice in guard clauses
              (table.insert condition (. pattern i)))
            (values `(let ,bindings ,condition) bindings))

          ;; multi-valued patterns (represented as lists)
          (list? pattern)
          (let [condition `(and)
                bindings []]
            (each [i pat (ipairs pattern)]
              (let [(subcondition subbindings) (match-pattern [(. vals i)] pat
                                                              unifications)]
                (table.insert condition subcondition)
                (each [_ b (ipairs subbindings)]
                  (table.insert bindings b))))
            (values condition bindings))
          ;; table patterns)
          (= (type pattern) :table)
          (let [condition `(and (= (type ,val) :table))
                bindings []]
            (each [k pat (pairs pattern)]
              (if (and (sym? pat) (= \"&\" (tostring pat)))
                  (do (assert (not (. pattern (+ k 2)))
                              \"expected rest argument before last parameter\")
                      (table.insert bindings (. pattern (+ k 1)))
                      (table.insert bindings [`(select ,k ((or _G.unpack table.unpack)
                                                           ,val))]))
                  (and (= :number (type k))
                       (= \"&\" (tostring (. pattern (- k 1)))))
                  nil ; don't process the pattern right after &; already got it
                  (let [subval `(. ,val ,k)
                        (subcondition subbindings) (match-pattern [subval] pat
                                                                  unifications)]
                    (table.insert condition subcondition)
                    (each [_ b (ipairs subbindings)]
                      (table.insert bindings b)))))
            (values condition bindings))
          ;; literal value
          (values `(= ,val ,pattern) []))))
  (fn match-condition [vals clauses]
    (let [out `(if)]
      (for [i 1 (length clauses) 2]
        (let [pattern (. clauses i)
              body (. clauses (+ i 1))
              (condition bindings) (match-pattern vals pattern {})]
          (table.insert out condition)
          (table.insert out `(let ,bindings ,body))))
      out))
  ;; how many multi-valued clauses are there? return a list of that many gensyms
  (fn val-syms [clauses]
    (let [syms (list (gensym))]
      (for [i 1 (length clauses) 2]
        (if (list? (. clauses i))
            (each [valnum (ipairs (. clauses i))]
              (if (not (. syms valnum))
                  (tset syms valnum (gensym))))))
      syms))
  ;; wrap it in a way that prevents double-evaluation of the matched value
  (let [clauses [...]
        vals (val-syms clauses)]
    (if (not= 0 (% (length clauses) 2)) ; treat odd final clause as default
        (table.insert clauses (length clauses) (sym :_)))
    ;; protect against multiple evaluation of the value, bind against as
    ;; many values as we ever match against in the clauses.
    (list (sym :let) [vals val]
          (match-condition vals clauses))))
 }")

(let [module-name "__fennel-bootstrap__"
      _ (tset package.preload module-name #m)
      env (specials.makeCompilerEnv nil specials.compilerScope {})
      built-ins (eval builtin-macros {:env env
                                      :scope specials.compilerScope
                                      :allowedGlobals false
                                      :useMetadata true
                                      :filename "fennel.fnl"
                                      :moduleName module-name})]
  (each [k v (pairs built-ins)]
    (tset compiler.globalScope.macros k v))
  (set compiler.globalScope.macros.λ compiler.globalScope.macros.lambda)
  (tset package.preload module-name nil))

m
