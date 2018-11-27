def before_setup(ctx):
    print "before_setup is called for following nodes: {0}".format(ctx['nodes'])

def after_setup(ctx):
    print "after_setup is called for following nodes: {0}".format(ctx['nodes'])

def before_boot_nodes(ctx):
    print "before_boot_nodes is called for following nodes: {0}".format(ctx['nodes'])

def after_boot_nodes(ctx):
    print "after_boot_nodes is called for following nodes: {0}".format(ctx['nodes'])

def before_startup(ctx):
    print "before_startup is called for following nodes: {0}".format(ctx['nodes'])

def after_startup(ctx):
    print "after_startup is called for following nodes: {0}".format(ctx['nodes'])
