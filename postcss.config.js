module.exports = context => {
  const node_env = (context.env == 'prod') ? 'prod' : 'dev'

  return {
    // from/to will be set via command line option when invoked from postcss-cli
    // See note here: https://github.com/postcss/postcss-cli#config
    plugins: {
      'postcss-import': {},
      'precss': {},
      'autoprefixer': {},
      'cssnano': (node_env == 'prod') ? {} : false,
    },
  }
}
