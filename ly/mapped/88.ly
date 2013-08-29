\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #88
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  bes'8 |
  bes4.~ bes4 c8 |
  bes4 g8 ees4 c8 |
  bes4.~ bes8[ ees g]( |
  bes4.) c |
  bes4.~ bes8 f8[ g] |
  aes4. c |
  
  bes~ bes8 ees,8[ f] |
  g4 b8\rest b4\rest bes8 |
  bes4.~ bes4 c8 |
  bes4 g8 ees4 c8 |
  bes4.~ bes8[ ees g]( |
  bes4.) ees |
  
  d4.~ d8 g,[ a] |
  bes4.~ bes8 a8[ bes] |
  g4. b4\rest b8\rest |
  b4\rest b8\rest b4\rest g8 |
  aes4 aes8 bes4 bes8 |
  c4 c8 d4 d8 |
  
  bes4.~ bes4 g8 |
  bes4. b4\rest g8 |
  bes4 bes8 a4 a8 |
  aes?4 aes8 f4 f8 |
  c'4.~ c8 bes8[ a] |
  bes4. b4\rest bes8 |
  
  bes4 bes8 c4 c8 |
  d4 d8 ees4 c8 |
  bes4.~ bes4 g8 |
  bes4. c |
  d4.~ d8 bes[ g] |
  f4.~ f8 ees[ c] |
  bes4. bes'4\rest bes8\rest | \break
  
  %page2/chorus
  bes4\rest bes8\rest bes4. |
  ees4.~ ees4 bes8 |
  ees4 bes8 d4 c8 |
  bes4.~ bes4 g8 |
  ees4. c' |
  
  bes4.~ bes8 f[ g] |
  aes4. c |
  bes~ bes8 ees,[ f] |
  g4.~ g4 \bar"" bes8 |
  ees4.~ ees4 bes8 |
  
  ees4 bes8 d4 c8 |
  bes4.~ bes4 g8 |
  ees4. g |
  aes4 g8 aes4 bes8 |
  c4. d\fermata |
  ees\fermata b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Of all __ the wives as e’er you know, __
  Yeo -- ho! lads, ho! Yeo -- ho! yeo -- ho!
  There’s none like Nan -- cy Lee, I trow, __
  Yeo -- ho! lads, ho! yeo -- ho!
  See there she stands and waves her hands up -- on __ the quay,
  An’ ev -- ’ry day when I’m a -- way,
  She’ll watch for __ me,
  An’ whis -- per low, when tem -- pests blow, for Jack __ at sea,
  Yeo -- ho! __ lads, ho! __ yeo -- ho!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The har -- bor’s past, the breez -- es blow, __
  Yeo -- ho! lads, ho! Yeo -- ho! yeo -- ho!
  ’Tis long ere we come back I know, __
  Yeo -- ho! lads, ho! yeo -- ho!
  But true and bright, from morn till night, my home will be,
  An’ all so neat, an’ snug, an’ sweet
  For Jack __ at __ sea,
  An’ Nan -- cy’s face to bless the place, an’ wel -- come me;
  Yeo -- ho! __ lads, ho! __ yeo -- ho!
  
  The sail -- or’s wife the sail -- or’s star __ shall be,
  Yeo -- ho! __ we go a -- cross the sea, __
  The sail -- or’s wife the sail -- or’s star shall be,
  The sail -- or’s wife his star shall be.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The boa -- ’s’n pipes the watch be -- low, __
  Yeo -- ho! lads, ho! Yeo -- ho! yeo -- ho!
  Then here’s a health be -- fore we go, __
  Yeo -- ho! lads, ho! yeo -- ho!
  A long, long life to my sweet wife, and mates at sea;
  An’ keep his bones from Da -- vy Jones
  Wher -- e’er __ you __ be,
  An’ may you meet a mate as sweet as Nan -- cy Lee,
  Yeo -- ho! __ lads, ho! __ yeo -- ho!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  g4.~ g4 g8 |
  g4 ees8 ees4 c8 |
  bes4.~ bes8[ ees g]~ |
  g4. g |
  f~ f8 d4 |
  f4. aes |
  
  g~ g8 ees4 |
  ees s4. g8 |
  g4.~ g4 g8 |
  g4 ees8 ees4 c8 |
  bes4.~ bes8[ ees g]~ |
  g4. g |
  
  g~ g8 g4 |
  g4.~ g8 fis4 |
  g4. s |
  s s4 ees8 |
  f4 f8 g4 g8 |
  aes4 aes8 aes4 aes8 |
  
  g4.~ g4 ees8 |
  g4. s4 ees8 |
  ees4 ees8 ees4 ees8 f4 f8 d4 d8 |
  aes'4.~ aes8 g[ fis] |
  g4. s4 g8 |
  
  aes4 aes8 aes4 aes8 |
  aes4 aes8 g4 aes8 |
  g4.~ g4 ees8 |
  g4. g |
  f~ f8 d4 |
  c4.~ c8 a4 |
  bes4. s |
  
  %chorus/page2
  s4. aes' |
  g~ g4 g8 |
  g4 g8 aes4 aes8 |
  g4.~ g4 ees8 |
  ees4. ees |
  
  d4.~ d8 d4 |
  f4. aes |
  g~ g8 ees4 |
  ees4.~ ees4 g8 |
  g4.~ g4 g8 |
  
  g4 g8 aes4 aes8 |
  g4.~ g4 ees8 |
  ees4. e |
  c4 ees?8 c4 ees8 |
  ees4. aes |
  g s4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes8 |
  bes4.~ bes4 bes8 |
  bes4 bes8 g4 aes8 |
  g4.~ g4( bes8)~ |
  bes4. ees |
  d4.~ d4 bes8 |
  d4. d |
  
  ees~ ees4 g,8 |
  bes4 s4. bes8 |
  bes4.~ bes4 bes8 |
  bes4 bes8 g4 aes8 |
  g4.~ g4( bes8)~ |
  bes4. bes |
  
  bes~ bes8 bes[ c] |
  d4.~ d8 c4 |
  bes4. s |
  s s4 bes8 |
  bes4 bes8 bes4 bes8 |
  bes4 bes8 bes4 bes8 |
  
  bes4.~ bes4 bes8 |
  bes4. s4 bes8 |
  g4 g8 ges4 ges8 |
  f4 f8 bes4 bes8 |
  bes4.~ bes8 bes4 |
  bes4. s4 bes8 |
  
  bes4 bes8 bes4 bes8 |
  bes4 bes8 bes4 ees8 |
  ees4.~ ees4 bes8 |
  ees4. c |
  bes~ bes8 bes4 |
  a4.~ a8 c4 |
  bes4. s |
  
  %page2/chorus
  
  s bes |
  bes~ bes4 bes8 |
  bes4 bes8 c4 d8 |
  ees4.~ ees4 bes8 |
  g4. g |
  
  f~ f8 f4 |
  bes4. bes |
  bes~ bes8 g[ aes] |
  bes4.~ bes4 bes8 |
  bes4.~ bes4 bes8 |
  
  bes4 bes8 c4 d8 |
  ees4.~ ees4 bes8 |
  g4. bes |
  aes4 c8 aes4 bes8 |
  aes4. bes |
  bes s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees4.~ ees4 ees8 |
  ees4 ees8 ees4 ees8 |
  ees4.~ ees4.~ |
  ees4. ees4. |
  bes'4.~ bes4
  bes8 |
  bes4. bes |
  
  ees,4.~ ees4 ees8  |
  ees4 d8\rest d4\rest ees8 |
  ees4.~ ees4 ees8 |
  ees4 ees8 ees4 ees8 |
  ees4.~ ees4.~ |
  ees4. c |
  
  d~ d8 d4 |
  d4.~ d8 d4 |
  g4. d4\rest d8\rest |
  d4\rest d8\rest d4\rest ees8 |
  d4 d8 bes4 bes8 |
  d4 d8 f4 f8 |
  
  ees4.~ ees4 ees8 |
  ees4. d4\rest ees8 |
  bes4 bes8 bes4 bes8 |
  bes4 bes8 bes4 bes8 |
  ees4.~ ees8 ees4 |
  ees4. d4\rest ees8 |
  
  d4 d8 bes4 bes8 |
  f'4 f8 ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. e |
  f~ f8 f4 |
  f4.~ f8 f4 |
  bes4. d,4\rest d8\rest |

  %chorus / page2
  d4\rest d8\rest bes'4. |
  ees,~ ees4 ees8 |
  ees4 ees8 ees4 ees8
  ees4.~ ees4 ees8 |
  ees4. ees |
  
  bes~ bes8 bes4 |
  bes4. d |
  ees4.~ ees8 ees4 |
  ees4.~ ees4 ees8 |
  ees4.~ ees4 ees8 |
  
  ees4 ees8 ees4 ees8 |
  ees4.~ ees4 ees8 |
  ees4. c |
  f4 c8 f4 g8 |
  aes4. f8( bes,4)\fermata |
  ees4.\fermata d4\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Nancy Lee"}}
  composer = \markup\oldStyleNum"Stephen Adams (1841–1913)"
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  tagline = ""
}}


global = {
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  a'4 |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar""
  
  a'4 |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar""
  
  a'4 |
  fis d fis a |
  d2 b4 cis8[ d] |
  cis4 a b gis |
  a2 \bar""
  
  b4 cis |
  d2 cis4 b |
  a2 b4 cis8[ d] |
  fis,4 fis g e |
  d2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh, where! and oh, where! is your High -- land lad -- die gone?
  Oh, where! and oh, where! is your High -- land lad -- die gone?
  He’s gone to fight the foe for King George up -- on the throne;
  And it’s oh! in my heart, how I wish him safe at home!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh, where! and oh, where! does your High -- land lad -- die dwell?
  Oh, where! and oh, where! does your High -- land lad -- die dwell?
  \set ignoreMelismata = ##t
  He dwelt in mer -- ry Scot -- land at the sign of the Blue Bell;
  \unset ignoreMelismata
  And it’s oh! in my heart, that I love my lad -- die well.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  What clothes, in what clothes is your High -- land lad -- die clad?
  What clothes, in what clothes is your High -- land lad -- die clad?
  His bon -- net’s Sax -- on green, and his waist -- coat of the plaid;
  And it’s oh! in my heart, that I love my High -- land lad.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Sup -- pose, and sup -- pose that your High -- land lad should die?
  Sup -- pose, and sup -- pose that your High -- land lad should die?
  The bag -- pipes shall play o’er him, I’d lay me down and cry;
  And it’s oh! in my heart, that I wish he may not die!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  fis4 |
  fis2 a4 g|
  fis2 g4 d |
  d d cis cis |
  d2.
  
  fis4 |
  fis2 a4 g |
  fis2 g4 d |
  d d cis cis |
  d2.
  
  fis4 |
  d4 d d e |
  d2 e4 e |
  e fis fis e |
  e2
  
  g4 g |
  fis2 a4 g |
  fis2 g4 d |
  d d e cis |
  d2. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a4 |
  a2 b4 cis |
  d2 g,4 a8[ b] |
  a4 a a g |
  fis2.
  
  a4 |
  a2 b4 cis |
  d2 g,4 a8[ b] |
  a4 a a g |
  fis2.
  
  a4 |
  a a a a |
  fis2 gis4 a8[ b] |
  a4 cis d d |
  cis2
  
  b4 a |
  a2 b4 cis |
  d2 d4 a8[ b] |
  a4 a a g |
  fis2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4 |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2.
  
  d4 |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2.
  
  d4 |
  d fis d cis |
  b2 e4 e |
  a, fis' d e |
  a,2
  
  e'4 e |
  d2 d4 d |
  d2 g4 g |
  a a, a a |
  d2. \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Blue Bells of Scotland"}}
  composer = \markup\oldStyleNum"Dorothea Jordan (1761–1816)"
  tagline = ""
}}


