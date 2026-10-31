/* SPDX-License-Identifier: GPL-3.0-or-later */
/**
 * @file suz/document.h
 * @brief Reading the structure of a session document.
 *
 * Nodes are addressed by position: segment index, token index within the
 * segment, morph index within the token, all counted from 0. Positions change
 * when the document is edited, so read them again after suz_perform(),
 * suz_undo() and suz_redo(). The identifiers returned by the *_id functions
 * are the ones used in commands.
 *
 * A position that does not exist is not an error of the session: count
 * functions return -1, functions with an output argument return -1, string
 * functions return NULL.
 */
#ifndef SUZ_DOCUMENT_H
#define SUZ_DOCUMENT_H

#include "suz/core.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @param session Session.
 * @return Number of segments.
 */
int suz_segment_count(suz_session *session);

/**
 * @param session Session.
 * @param segment Segment index.
 * @return Number of tokens in the segment, or -1.
 */
int suz_token_count(suz_session *session, int segment);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @return Number of morphs in the token, or -1.
 */
int suz_morph_count(suz_session *session, int segment, int token);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param id Receives the segment identifier.
 * @return 0 on success, -1 if the position does not exist.
 */
int suz_segment_id(suz_session *session, int segment, uint64_t *id);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param id Receives the token identifier.
 * @return 0 on success, -1 if the position does not exist.
 */
int suz_token_id(suz_session *session, int segment, int token, uint64_t *id);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @param id Receives the morph identifier.
 * @return 0 on success, -1 if the position does not exist.
 */
int suz_morph_id(suz_session *session, int segment, int token, int morph, uint64_t *id);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @return Surface form of the token, release with suz_free(), or NULL.
 */
char *suz_token_surface(suz_session *session, int segment, int token);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @return Form of the morph, release with suz_free(), or NULL.
 */
char *suz_morph_form(suz_session *session, int segment, int token, int morph);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @return 1 if the morph has a gloss, 0 if it has none, -1 if the position
 * does not exist.
 */
int suz_morph_glossed(suz_session *session, int segment, int token, int morph);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @return Lexical part of the gloss, release with suz_free(). NULL if the
 * morph has no gloss or the position does not exist.
 */
char *suz_morph_lexical(suz_session *session, int segment, int token, int morph);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @return Number of grammatical tags in the gloss, 0 if the morph has no
 * gloss, -1 if the position does not exist.
 */
int suz_morph_tag_count(suz_session *session, int segment, int token, int morph);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @param tag Tag index.
 * @return Grammatical tag, release with suz_free(), or NULL.
 */
char *suz_morph_tag(suz_session *session, int segment, int token, int morph, int tag);

/**
 * Universal Dependencies columns of a morph, see suz_morph_ud_text().
 */
#define SUZ_UD_LEMMA 0
#define SUZ_UD_UPOS 1
#define SUZ_UD_XPOS 2
#define SUZ_UD_FEATS 3
#define SUZ_UD_DEPREL 4
#define SUZ_UD_DEPS 5
#define SUZ_UD_MISC 6

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @return 1 if the morph has a UD annotation, 0 if it has none, -1 if the
 * position does not exist.
 */
int suz_morph_has_ud(suz_session *session, int segment, int token, int morph);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @param field One of the SUZ_UD_ constants.
 * @return Text of the CoNLL-U column, empty when the column is not given,
 * release with suz_free(). NULL if the morph has no UD annotation, the field
 * is not one of the constants or the position does not exist.
 */
char *suz_morph_ud_text(suz_session *session, int segment, int token, int morph, int field);

/**
 * @param session Session.
 * @param segment Segment index.
 * @param token Token index.
 * @param morph Morph index.
 * @param id Receives the identifier of the head morph when the result is 2.
 * @return 0 if the HEAD column is not given, 1 if the morph is the root of
 * its sentence, 2 if it depends on another morph, -1 if the morph has no UD
 * annotation or the position does not exist.
 */
int suz_morph_ud_head(suz_session *session, int segment, int token, int morph, uint64_t *id);

#ifdef __cplusplus
}
#endif

#endif
