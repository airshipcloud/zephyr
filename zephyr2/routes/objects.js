
/*
 * GET home page.
 */

exports.objects = function(req, res){
  var id = req.params[0];
   console.log(id);
  res.render('objects', { title: 'Express' });
};