#[cfg(test)]
mod cranelift_backend_tests {
    use std::fs;

    use cranelift::{
        codegen::{gimli::X86_64, ir::FuncRef},
        module::{DataDescription, Linkage, Module},
        object::ObjectModule,
        prelude::{
            isa::{CallConv, OwnedTargetIsa},
            settings::{self, Flags},
            types, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, Type,
            Value,
        },
    };

    #[test]
    fn test_create_object() {
        let isa_builder =
            cranelift::native::builder_with_options(true).expect("failed to make isa builder");
        let flags_builder = settings::builder();
        let isa = isa_builder
            .finish(Flags::new(flags_builder))
            .expect("failed to make isa");
        let object_builder = cranelift::object::ObjectBuilder::new(
            isa,
            "test_object",
            cranelift::module::default_libcall_names(),
        )
        .expect("failed to make builder");

        let mut object = ObjectModule::new(object_builder);
        let ptr_type = object.target_config().pointer_type();

        let mut puts_signature = object.make_signature();
        puts_signature.params.push(AbiParam::new(ptr_type));

        let hello_world_str_id = object
            .declare_data("hello_world_str", Linkage::Local, false, false)
            .expect("failed to declare hello_world_str");
        let mut hello_world_str_contents = DataDescription::new();
        hello_world_str_contents.define(Box::from(c"Hello world".to_bytes()));
        object
            .define_data(hello_world_str_id, &hello_world_str_contents)
            .expect("failed to define hello_world_str");

        let puts_id = object
            .declare_function("puts", Linkage::Import, &puts_signature)
            .expect("failed to declare puts");

        // same context should be reused for multiple functions
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut ctx = object.make_context();
        ctx.func.signature.params.push(AbiParam::new(types::I32));
        ctx.func.signature.params.push(AbiParam::new(ptr_type));

        ctx.func.signature.returns.push(AbiParam::new(types::I32));

        let hello_world_str_val = object.declare_data_in_func(hello_world_str_id, &mut ctx.func);
        let puts_ref = object.declare_func_in_func(puts_id, &mut ctx.func);

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

        let entry = fn_builder.create_block();
        fn_builder.append_block_params_for_function_params(entry);
        fn_builder.switch_to_block(entry);

        let ptr_to_hello_world = fn_builder.ins().global_value(ptr_type, hello_world_str_val);
        fn_builder.ins().call(puts_ref, &[ptr_to_hello_world]);

        let five = fn_builder.ins().iconst(types::I32, 5);
        fn_builder.ins().return_(&[five]);

        fn_builder.seal_block(entry);

        fn_builder.finalize();
        ctx.verify(object.isa()).expect("verification failed");

        let main_id = object
            .declare_function(
                "main",
                cranelift::module::Linkage::Export,
                &ctx.func.signature,
            )
            .expect("failed to declare function");

        object
            .define_function(main_id, &mut ctx)
            .expect("failed to define function");

        let finished_object = object.finish();
        let bytes = finished_object.emit().expect("failed to emit object file");

        fs::write("test.o", bytes).expect("failed to write to object file");
    }
}
