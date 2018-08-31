let check_result checkable label got expected () =
  Alcotest.(check checkable)
    label
    got
    expected

let test_case
    ~label
    ?(speed=`Quick)
    ?(descr="")
    checkable
    got
    expected =
  Alcotest.(
    test_case
      label
      speed
      (check_result checkable descr got expected)
  )

let hooray =
  [ test_case
      ~label:"hooray"
      ~descr:"this is just a test to prove we can compile"
      Alcotest.string
      "the result"
      "the result"
  ; test_case
      ~label:"jwt"
      Alcotest.string
      Notify.Session.(
        create ~api_key:"api_key" ~service_id:"service_id" ()
        |> create_jwt)
      "the result"
  ]


let () =
  Alcotest.run "notify"
    [ "main", hooray
    ]
