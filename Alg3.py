import copy


def algorithm_3(graph, source, target):
    # setup:
    # source = graph.vertex(source)
    # target = graph.vertex(target)

    # initialization
    pointer = 0
    num = 1
    tags = [0]  # tags is an array of integers, was 1 before, python starts counting from 0
    flag = False

    net = graph.get_adjlist(mode='in')
    all_MFRs = [[[target, net[target]]]]
    # all_MFRs.append([[target, net[target]]]) # all_MFRs[0] = [net[[0], [target]], net[[1], [target]]]

    # while some partial MFRs remain
    while pointer <= num:
        flag = False
        c_MFR = all_MFRs[pointer]
        c_tag = tags[pointer]  # current tag is integer
        # while the current MFR is incomplete
        while not flag:
            c_node = c_MFR[c_tag][0]
            c_preds = c_MFR[c_tag][1]  # c_node.predecessors, c_preds is a list

            # if no predecessors remain
            if not c_preds:  # python uses implicit booleans for lists
                if c_tag == len(c_MFR) - 1:
                    flag = True
                else:
                    c_tag = c_tag + 1

            if True:  # placeholder, actually need to check that c_node is not composite through igraph?
                m = len(c_preds)
                c_MFR[c_tag][1] = c_preds[0]

                i = 0
                # DMFRS keeps track of order of appending mfrs
                while i < m - 1:
                    temp1 = copy.deepcopy(c_MFR)  # Python doesn't reassign the actual list otherwise 
                    # deepcopy is very memory-intensive
                    temp1[c_tag][1] = c_preds[i + 1]
                    # current MFR is replaced by first copy
                    all_MFRs.append(temp1)  # all_MFRs[num + i] = temp1
                    tags.append(c_tag)  # tags[num + i] = c_tag
                    i = i + 1
                num = num + m - 1
            c_preds = [c_MFR[c_tag][1]]

            allin = True

            if allin:  # placeholder, needs to check that all c_preds are in c_mfr. Note this changes c_preds
                if c_tag == len(c_MFR) - 1:  # same as line 35
                    flag = True
                else:
                    c_tag = c_tag + 1
            else:
                for v in c_preds:  # isn't it implied that v is not in cmfr?
                    temp2 = net[v]
                    c_MFR.append([v, temp2])

            c_tag = c_tag + 1

        pointer = pointer + 1
