extends SceneTree
## Run with a dev-observability extension and its authored app profile scene.
## godot --headless --path PROJECT --script THIS_FILE -- --measure-startup res://profile.tscn


class ScriptedLevel extends SceneRoot:
	var calls: Array[String] = []

	func _enter_tree() -> void:
		calls.append("enter")

	func _ready() -> void:
		calls.append("ready")

	func _exit_tree() -> void:
		calls.append("exit")

	func _notification(what: int) -> void:
		if what == NOTIFICATION_READY:
			calls.append("notification")


func _initialize() -> void:
	_run.call_deferred()


func _run() -> void:
	var args := OS.get_cmdline_user_args()
	if (
		args.size() != 2
		or args[0] != "--measure-startup"
		or OS.has_environment("EQUILIBRIA_BENCHMARK_OUTPUT_FILE")
	):
		push_error("Pass --measure-startup and the profile scene path. Unset the benchmark output override.")
		quit(1)
		return
	var profile := load(args[1]) as PackedScene
	if profile == null:
		quit(1)
		return

	# Two readers keep their original app ID so we can inspect it after removal.
	var level := ScriptedLevel.new()
	level.add_child(profile.instantiate())
	var attached := BenchmarkSceneDirector.new()
	var detached := BenchmarkSceneDirector.new()
	var prefix := "user://scene_root_notifications_%s_%s_" % [OS.get_process_id(), Time.get_ticks_usec()]
	attached.output_file_path = prefix + "attached.yml"
	detached.output_file_path = prefix + "detached.yml"
	level.add_child(attached)
	level.add_child(detached)
	root.add_child(level)

	attached.finish_now()
	root.remove_child(level)
	detached.finish_now()

	# The script receives every callback. The bridge records readiness and releases the capture.
	var attached_report := FileAccess.get_file_as_string(attached.output_file_path)
	var detached_report := FileAccess.get_file_as_string(detached.output_file_path)
	var entered := attached_report.contains('name: "scene attachment"')
	var released := detached_report.contains("capture not requested before level entry")
	var callbacks := level.calls
	callbacks.sort()
	DirAccess.remove_absolute(attached.output_file_path)
	DirAccess.remove_absolute(detached.output_file_path)
	level.free()
	if not entered or not released or callbacks != ["enter", "exit", "notification", "ready"]:
		push_error("SceneRoot lifecycle failed. Recorded %s, released %s, callbacks %s" % [entered, released, callbacks])
		quit(1)
		return
	quit(0)
