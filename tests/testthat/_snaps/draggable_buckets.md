# Snapshot test for ui component dragable buckets

    Code
      as.character(draggable_buckets("my_input_id", "test_label", "element_1", "buckets_1"))
    Output
      [1] "<div class=\"draggableBuckets\" id=\"my_input_id\">\n  <span>test_label</span>\n  <div id=\"my_input_idelements\" class=\"form-control elements\" ondragover=\"allowDrop(event)\" ondrop=\"drop(event)\" data-widget=\"my_input_id\">\n    <div id=\"my_input_iddraggable1\" class=\"element\" draggable=\"true\" ondragstart=\"drag(event)\" ondragover=\"allowDrop(event)\" ondrop=\"dropReorder(event)\" data-widget=\"my_input_id\">element_1</div>\n  </div>\n  <div class=\"form-control bucket\" ondragover=\"allowDrop(event)\" ondrop=\"drop(event)\" data-label=\"buckets_1\" data-widget=\"my_input_id\">\n    <div class=\"bucket-name\" ondragover=\"allowDrop(event)\" ondrop=\"dropBucketName(event)\" data-widget=\"my_input_id\">buckets_1:</div>\n  </div>\n</div>"

