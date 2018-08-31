let main () =
  let session = 
    Notify.Session.create
      ~service_id:"27ba9f2a-2803-4c8e-82c1-1357de4b6a94"
      ~api_key:"447000ea-623b-48dc-928f-aef73346894a"
      ()
  in

  Lwt.(
    Notify.get_all_templates session >|= Printf.printf "wow. %s"
  )

let () =
  Lwt_main.run @@ main ()

