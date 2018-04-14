open CommandUtils

let init ~on_spawn root =
  let flowconfig = FlowConfig.empty_config in
  let monitor_log_file = "monitor.log" in
  let server_log_file = "server.log" in
  (* let root = guess_root dir_or_file in *)
  let options_flags = {
    Options_flags.
    all = false;
    debug = false;
    flowconfig_flags = {
      ignores = [];
      untyped = [];
      includes = [];
      libs = [];
      raw_lint_severities = [];
    };
    include_warnings = false;
    max_warnings = None;
    max_workers = None;
    munge_underscore_members = false;
    no_flowlib = false;
    profile = false;
    quiet = false;
    strip_root = false;
    temp_dir = None;
    traces = None;
    verbose = None;
    weak = false;
    merge_timeout = None;
    file_watcher = Options.NoFileWatcher;
  }
  in
  let server_options = make_options ~flowconfig ~lazy_mode:None ~root options_flags in
  let shm_flags = {
    shm_dirs = None;
    shm_min_avail = None;
    shm_dep_table_pow = None;
    shm_hash_table_pow = None;
    shm_log_level = None;
  }
  in
  let shared_mem_config = shm_config shm_flags flowconfig in
  let monitor_options = FlowServerMonitorOptions.make
      ~log_file:monitor_log_file
      ~autostop:false
      ~no_restart:true
      ~server_log_file
      ~server_options
      ~shared_mem_config
      ~argv:[||]
  in
  FlowServerMonitor.daemonize ~wait:false ~on_spawn monitor_options
