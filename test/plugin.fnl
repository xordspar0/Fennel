(local l (require :test.luaunit))
(local fennel (require :fennel))
(local view (require :fennelview))

(fn test-plugin []
  (var hook-count 0)

  (fn test-destructure [from to]
    (set hook-count (+ hook-count 1))
    (l.assertEquals from 1)
    (l.assertEquals (tostring to) "x")
    (let [(from2 to2) (coroutine.yield)]
      (l.assertEquals from2 2)
      (l.assertEquals (tostring to2) "y"))
    (let [(from3 to3 scope) (coroutine.yield)]
      (l.assertEquals (view from3) "(fn [a b] 0)")
      (l.assertEquals (view to3) "f")
      (l.assertEquals scope.manglings {:x "x" :y "y"})))

  (fn test-call [ast scope]
    (set hook-count (+ hook-count 1))
    (l.assertEquals (tostring ast) "(f 3 (+ x y))")
    (l.assertEquals scope.manglings {:f "f" :x "x" :y "y"}))

  (fn test-ste [symbol scope]
    (set hook-count (+ hook-count 1))
    (l.assertEquals (tostring symbol) "f")
    (l.assertEquals scope.manglings {:f "f" :x "x" :y "y"})
    (let [symbol2 (coroutine.yield)]
      (l.assertEquals (tostring symbol2) "x"))
    (let [symbol3 (coroutine.yield)]
      (l.assertEquals (tostring symbol3) "y")))

  (fn test-destructure2 [from to scope]
    (set hook-count (+ hook-count 1))
    (coroutine.yield)
    (coroutine.yield))

  (let [plugin1 {:destructure (coroutine.wrap test-destructure)
                 :call (coroutine.wrap test-call)
                 :symbol-to-expression (coroutine.wrap test-ste)}
        plugin2 {:destructure (coroutine.wrap test-destructure2)}
        code "(let [x 1 y 2 f (fn [a b] 0)] (f 3 (+ x y)))"]
    (fennel.eval code {:plugins [plugin1 plugin2]})
    (l.assertEquals hook-count 4 "Not all the plugins ran!")
    (each [_ hook (pairs plugin1)]
      (let [(ok? msg) (pcall hook)]
        (l.assertStrContains msg "cannot resume dead coroutine")))))

(fn test-unused-locals []
  ;; copied from --check-unused-locals in the bin script
  (let [unused-cases {"(fn [xx y] y)" "xx"
                      "(fn [_x y z] y)" "z"
                      "(let [x 1 y 2] y)" "x"}
        hook (fennel.eval "(fn [ast scope]
  (each [symname (pairs scope.symmeta)]
    (assert-compile (or (. scope.symmeta symname :used)
                        (symname:find \"^_\"))
                    (: \"unused local %s\" :format symname) ast)))"
                          {:env "COMPILER"})]
    (each [code name (pairs unused-cases)]
      (let [(ok? msg) (pcall fennel.eval code {:plugins [{:do hook :fn hook}]})]
        (l.assertNot ok? "Should catch unused locals")
        (l.assertStrContains (.. "unused local " name) msg)))))

{: test-plugin}
