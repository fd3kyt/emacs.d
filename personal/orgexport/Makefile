EMACS=emacsclient   # 这里我用的emcasclient, 没有用emacs. 因为我的emacs是以server方式启动的
ORG_CONFIG_FILE=publish-config.el   # 导出org文件的配置
EMACS_OPTS=--eval "(load-file \"$(ORG_CONFIG_FILE)\")"

OUTPUT_DIR=~/publish/output   # 导出的位置, 这个位置其实是在 public-config.el 中配置的,
			  # 这里的定义这个变量的作用是为了删除(make clean), 以及上传server(make upload)

all:
	@echo "Generating HTML..."
	@mkdir -p $(OUTPUT_DIR)
	@$(EMACS) $(EMACS_OPTS)
	@echo "HTML generation done"

clean:
	@rm -rf $(OUTPUT_DIR)
