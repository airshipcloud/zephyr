
/*
 * GET home page.
 */

exports.index = function(req, res){
  var id = req.params[0]; 
  res.render('index', { title: 'Express' });
};