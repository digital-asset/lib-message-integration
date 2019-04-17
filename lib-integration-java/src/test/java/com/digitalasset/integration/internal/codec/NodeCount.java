// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.integration.internal.codec;

import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Test functions for comparing node counts.
 * NOTE: currently it's possible that some text encodings could be slightly different, e.g. dates and times
 * therefore we cannot simply do a text diff.
 */
public class NodeCount {

    // counts elements, attributes and text nodes
    public static Map<String, Integer> getNodeCounts(Element root) {
        Map<String, Integer> counts = Collections.singletonMap(root.getTagName(), 1);
        NodeList list = root.getChildNodes();
        for(int i=0; i<list.getLength(); i++) {
            Node item = list.item(i);
            if(item.getNodeType()==Element.ELEMENT_NODE) {
                counts = mergeMaps(counts, getNodeCounts((Element)item));
            } else if (item.getNodeType()==Element.ATTRIBUTE_NODE) {
                counts = mergeMaps(counts, Collections.singletonMap(((Attr)item).getName(), 1));
            } else if (item.getNodeType()==Element.TEXT_NODE) {
                if(StringUtils.isNotBlank(item.getTextContent())) {
                    counts = mergeMaps(counts, Collections.singletonMap("<TEXT>", 1));
                }
            }
        }
        return counts;
    }

    public static Map<String, Integer> mergeMaps(Map<String, Integer> m1, Map<String, Integer> m2) {
        Map<String, Integer> res = new HashMap<>(m2);
        m1.forEach((k, v) -> res.merge(k, v, (x, y) -> x + y));
        return res;
    }
}
