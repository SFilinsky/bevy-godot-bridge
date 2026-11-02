extends SceneTree
## Run with --script THIS_FILE -- res://authored_app_profile.tscn.
## The fixture orders consumers before providers and moves them between two levels.


class EarlyConsumer extends Node:
	var expected: BevyApp
	var found_during_entry := false

	func _enter_tree() -> void:
		found_during_entry = BevyApp.resolve_or_null(self) == expected


class Initializer extends Node:
	var calls := 0

	func initialize() -> void:
		calls += 1


func _initialize() -> void:
	_run.call_deferred()


func _run() -> void:
	var profile := load(OS.get_cmdline_user_args()[0]) as PackedScene
	var outer := SceneRoot.new()
	var wrapper := Node.new()
	var consumer := EarlyConsumer.new()
	var provider := profile.instantiate()
	consumer.expected = provider.get_node("BevyApp") as BevyApp
	wrapper.add_child(consumer)
	wrapper.add_child(provider)
	outer.add_child(wrapper)
	var nested := SceneRoot.new()
	var nested_provider := profile.instantiate()
	nested.add_child(nested_provider)
	outer.add_child(nested)
	var nested_app := nested_provider.get_node("BevyApp") as BevyApp

	# Detached scenes already have all their authored children.
	var ok := BevyApp.resolve_or_null(consumer) == consumer.expected
	root.add_child(outer)
	ok = ok and consumer.found_during_entry and BevyApp.resolve_or_null(nested) == nested_app

	# A mounted subtree may contain an app after an early consumer.
	var second := SceneRoot.new()
	root.add_child(second)
	wrapper.reparent(second)
	ok = ok and consumer.found_during_entry and BevyApp.resolve_or_null(second) == consumer.expected
	# Removing the outer provider leaves only a nested one. It must stay invisible here.
	var initializer := Initializer.new()
	outer.add_child(initializer)
	InitializationCoordinator.register_pre_import_initializer_node(initializer)
	ok = ok and initializer.calls == 1

	# After replacement, a duplicate is rejected, and removal restores the single result.
	root.remove_child(second)
	var other := BevyApp.new()
	second.add_child(other)
	ok = ok and BevyApp.resolve_or_null(consumer) == null
	second.remove_child(other)
	other.free()
	ok = ok and BevyApp.resolve_or_null(consumer) == consumer.expected
	outer.free()
	second.free()
	if not ok:
		push_error("Scoped service lookup failed")
		quit(1)
		return
	print("Scoped service lookup passed")
	quit()